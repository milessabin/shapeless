package shapeless

import scala.tools.nsc.Global
import scala.tools.nsc.plugins._
import scala.tools.nsc.transform._

class ShapelessPlugin(val global: Global) extends Plugin { self =>
  import global._

  val name = "shapeless-plugin"
  val description = "Replaces shapeless.Lazy and shapeless.Strict with by-name implicits on Scala 2.13"
  val components: List[PluginComponent] = ShapelessComponent :: Nil

  private object ShapelessComponent extends PluginComponent with Transform {
    override val global: self.global.type = self.global
    override val runsAfter = "parser" :: Nil
    override val runsBefore = "namer" :: Nil
    override val phaseName = self.name
    override def newTransformer(unit: CompilationUnit) = ShapelessTransformer

    object ShapelessTransformer extends Transformer {
      private[this] val Shapeless = TermName("shapeless")
      private[this] val Lazy = TypeName("Lazy")
      private[this] val Strict = TypeName("Strict")
      private[this] val Value = TermName("value")
      private[this] var lazyParams: Set[Name] = Set.empty
      private[this] var strictParams: Set[Name] = Set.empty

      private object ShapelessType {
        def unapply(tree: RefTree): Option[TypeName] = tree match {
          case Ident(name: TypeName) => Some(name)
          case Select(Ident(Shapeless), name: TypeName) => Some(name)
          case Select(Select(Ident(nme.ROOTPKG), Shapeless), name: TypeName) => Some(name)
          case _ => None
        }
      }

      override def transform(tree: Tree): Tree = tree match {
        // def name[..tparams](...params)(implicit ip, ..): tpt = rhs
        case DefDef(mods, name, tparams, paramss :+ (implicits @ List(ip, _*)), tpt, rhs) if ip.mods.isImplicit =>
          val restoreLazyParams = lazyParams
          val restoreStrictParams = strictParams
          try {
            val adaptedImplicits = implicits.mapConserve(adaptImplicit)
            val adaptedTpt = transform(tpt)
            val adaptedRhs = transform(rhs)
            if (adaptedImplicits.eq(implicits) && adaptedTpt.eq(tpt) && adaptedRhs.eq(rhs)) tree
            else treeCopy.DefDef(tree, mods, name, tparams, paramss :+ adaptedImplicits, adaptedTpt, adaptedRhs)
          } finally {
            lazyParams = restoreLazyParams
            strictParams = restoreStrictParams
          }

        // param.value.T where param is a by-name implicit
        case Select(Select(param @ Ident(name), Value), tpe: TypeName) if lazyParams(name) =>
          val message = s"By-name implicit parameter $name is not a stable identifier. Refactor to use the Aux pattern."
          reporter.error(tree.pos, message)
          Select(param, tpe)

        // param.value
        case Select(param @ Ident(name), Value) if lazyParams(name) || strictParams(name) =>
          param

        case _ =>
          super.transform(tree)
      }

      private def adaptImplicit(param: ValDef): ValDef = param.tpt match {
        // Lazy[T]
        case AppliedTypeTree(ShapelessType(Lazy), targs @ List(_)) =>
          lazyParams += param.name
          val mods = param.mods | Flag.BYNAMEPARAM
          val tpt = AppliedTypeTree(gen.rootScalaDot(tpnme.BYNAME_PARAM_CLASS_NAME), targs)
          treeCopy.ValDef(param, mods, param.name, tpt, param.rhs)

        // Strict[T]
        case AppliedTypeTree(ShapelessType(Strict), List(targ)) =>
          strictParams += param.name
          treeCopy.ValDef(param, param.mods, param.name, targ, param.rhs)

        case _ =>
          param
      }
    }
  }
}

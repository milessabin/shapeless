package shapeless

import scala.tools.nsc.Global
import scala.tools.nsc.plugins._
import scala.tools.nsc.transform._

class ShapelessPlugin(val global: Global) extends Plugin { self =>
  import global._

  val name = "shapeless-plugin"
  val description = "Replaces by-name implicits with shapeless.Lazy on Scala 2.12"
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
      private[this] val Value = TermName("value")
      private[this] var byNameParams: Set[Name] = Set.empty

      override def transform(tree: Tree): Tree = tree match {
        // def name[..tparams](...params)(implicit ip, ..): tpt = rhs
        case DefDef(mods, name, tparams, paramss :+ (implicits @ List(ip, _*)), tpt, rhs) if ip.mods.isImplicit =>
          val restoreByNameParams = byNameParams
          try {
            val adaptedImplicits = implicits.mapConserve(adaptImplicit)
            val adaptedTpt = transform(tpt)
            val adaptedRhs = transform(rhs)
            if (adaptedImplicits.eq(implicits) && adaptedTpt.eq(tpt) && adaptedRhs.eq(rhs)) tree
            else treeCopy.DefDef(tree, mods, name, tparams, paramss :+ adaptedImplicits, adaptedTpt, adaptedRhs)
          } finally {
            byNameParams = restoreByNameParams
          }

        // param.T where param is a by-name implicit
        case Select(param @ Ident(name), tpe: TypeName) if byNameParams(name) =>
          Select(Select(param, Value), tpe)

        // param where param is a by-name implicit
        case param @ Ident(name) if byNameParams(name) =>
          Select(param, Value)

        case _ =>
          super.transform(tree)
      }

      private def adaptImplicit(param: ValDef): ValDef = param.tpt match {
        // => T
        case AppliedTypeTree(ref: RefTree, targs @ List(_)) if ref.name == tpnme.BYNAME_PARAM_CLASS_NAME =>
          byNameParams += param.name
          val mods = param.mods &~ Flag.BYNAMEPARAM
          val tpt = AppliedTypeTree(Select(gen.rootId(Shapeless), Lazy), targs)
          treeCopy.ValDef(param, mods, param.name, tpt, param.rhs)

        case _ =>
          param
      }
    }
  }
}

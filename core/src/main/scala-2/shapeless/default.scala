package shapeless

import scala.language.experimental.macros
import scala.reflect.macros.whitebox

trait DefaultScalaCompat {
  implicit def materialize[T, L <: HList]: Default.Aux[T, L] = macro DefaultMacros.materialize[T, L]
}

trait DefaultAsRecordScalaCompat {
  implicit def asRecord[T, Labels <: HList, Options <: HList](
    implicit
    default: Default.Aux[T, Options],
    labelling: Labelling.Aux[T, Labels],
    helper: Default.AsRecord.Helper[Options, Labels]
  ): Default.AsRecord.Aux[T, helper.Out] = new Default.AsRecord[T] {
    type Out = helper.Out
    def apply(): Out = helper(default())
  }
}

class DefaultMacros(val c: whitebox.Context) extends CaseClassMacros {
  import c.universe._

  def someTpe = typeOf[Some[_]].typeConstructor
  def noneTpe = typeOf[None.type]

  def materialize[T: WeakTypeTag, L: WeakTypeTag]: Tree = {
    val tpe = weakTypeOf[T]
    val cls = classSym(tpe)

    lazy val companion = companionRef(tpe)
    def altCompanion = companion.symbol.info

    val none = q"_root_.scala.None"
    def some(value: Tree) = q"_root_.scala.Some($value)"

    // Symbol.alternatives is missing in Scala 2.10
    def overloadsOf(sym: Symbol) =
      if (sym.isTerm) sym.asTerm.alternatives
      else if (sym.isType) sym :: Nil
      else Nil

    def hasDefaultParams(method: MethodSymbol) =
      method.paramLists.flatten.exists(_.asTerm.isParamWithDefault)

    // The existence of multiple apply overloads with default values gets checked
    // after the macro runs. Their existence can make the macro expansion fail,
    // as multiple overloads can define the functions we look for below, possibly
    // with wrong types, making the compilation fail with the wrong error.
    // We do this check here to detect that beforehand.
    def overloadsWithDefaultParamsIn(tpe: Type) =
      overloadsOf(tpe.member(TermName("apply"))).count {
        alt => alt.isMethod && hasDefaultParams(alt.asMethod)
      }

    def defaultsFor(fields: List[(TermName, Type)]) = for {
      ((_, argTpe), i) <- fields.zipWithIndex
      default = tpe.companion.member(TermName(s"apply$$default$$${i + 1}")) orElse
        altCompanion.member(TermName(s"$$lessinit$$greater$$default$$${i + 1}"))
    } yield if (default.isTerm) {
      val defaultTpe = appliedType(someTpe, devarargify(argTpe))
      val defaultVal = some(q"$companion.$default")
      (defaultTpe, defaultVal)
    } else (noneTpe, none)

    def mkDefault(defaults: List[(Type, Tree)]) = {
      val (types, values) = defaults.unzip
      val outTpe = mkHListTpe(types)
      val outVal = mkHListValue(values)
      q"_root_.shapeless.Default.mkDefaultByName[$tpe, $outTpe]($outVal)"
    }

    if (isCaseObjectLike(cls)) return mkDefault(Nil)
    if (!isCaseClassLike(cls)) abort(s"$tpe is not a case class or case class like")

    // ClassSymbol.primaryConstructor is missing in Scala 2.10
    val primaryCtor = overloadsOf(tpe.decl(termNames.CONSTRUCTOR)).find {
      alt => alt.isMethod && alt.asMethod.isPrimaryConstructor
    }.getOrElse {
      c.abort(c.enclosingPosition, s"Cannot get primary constructor of $tpe")
    }.asMethod

    // Checking if the primary constructor has default parameters, and returning
    // a Default instance with non-empty types / values only if that holds.
    // The apply$default$... methods below may still exist without these, if an additional
    // apply method has default parameters. We want to ignore them in this case.
    val hasUniqueDefaults = hasDefaultParams(primaryCtor) && {
      val k = overloadsWithDefaultParamsIn(tpe.companion)
      k == 1 || (k == 0 && overloadsWithDefaultParamsIn(altCompanion) == 1)
    }

    mkDefault {
      val fields = fieldsOf(tpe)
      if (hasUniqueDefaults) defaultsFor(fields)
      else List.fill(fields.size)((noneTpe, none))
    }
  }
}

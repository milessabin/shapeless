package shapeless

import scala.language.experimental.macros
import scala.reflect.macros.whitebox

abstract class Derived[+T] extends Serializable {
  def value: T
}

object Derived {
  def apply[T](t: T): Derived[T] =
    new Derived[T] {
      def value = t
    }

  implicit def derive[T]: Derived[T] = macro DerivedMacros.derive[T]
}

@macrocompat.bundle
class DerivedMacros(val c: whitebox.Context) extends CaseClassMacros with OpenImplicitMacros {
  import c.universe._

  lazy val typeClassTpe = typeOf[TypeClass[Any]].typeConstructor
  lazy val labelledTypeClassTpe = typeOf[LabelledTypeClass[Any]].typeConstructor
  lazy val productTypeClassTpe = typeOf[ProductTypeClass[Any]].typeConstructor
  lazy val labelledProductTypeClassTpe = typeOf[LabelledProductTypeClass[Any]].typeConstructor

  lazy val genericTpe = typeOf[Generic[_]].typeConstructor

  def doDerive(instanceTpe: Type, tcTpe: Type, paramTpe: Type) = {

    val isProduct0 =
      if (isProduct(paramTpe))
        true
      else if (isCoproduct(paramTpe))
        false
      else
        abort(s"$paramTpe is not case class like or the root of a sealed family of types")

    def typeClassInstanceOpt(labelled: Boolean): Option[Tree] = {

      val baseTypeClassTpe =
        if (labelled) {
          if (isProduct0)
            labelledProductTypeClassTpe
          else
            labelledTypeClassTpe
        } else {
          if (isProduct0)
            productTypeClassTpe
          else
            typeClassTpe
        }

      val appliedTypeClassTpe = appliedType(baseTypeClassTpe, tcTpe)

      c.inferImplicitValue(appliedTypeClassTpe, silent = true) match {
        case EmptyTree => None
        case tree => Some(tree)
      }
    }

    val (tc, isLabelled) =
      typeClassInstanceOpt(labelled = false).map((_, false))
        .orElse(typeClassInstanceOpt(labelled = true).map((_, true)))
        .getOrElse {
          abort(s"Cannot find a *TypeClass instance for $tcTpe")
        }

    val genType = appliedType(genericTpe, paramTpe)
    val gen = c.inferImplicitValue(genType)

    val paramLabelTpes =
      if (isProduct0) {
        if (paramTpe =:= typeOf[Unit] || isCaseObjectLike(paramTpe.typeSymbol.asClass))
          Nil
        else
          fieldsOf(paramTpe).map { case (name, tpe0) => nameAsString(name) -> devarargify(tpe0) }
      } else
        ctorsOf(paramTpe).map { tpe0 => nameAsString(nameOf(tpe0)) -> tpe0 }

    val pointMethod = TermName(if (isProduct0) "emptyProduct" else "emptyCoproduct")
    val prodMethod = TermName(if (isProduct0) "product" else "coproduct")

    val t = paramLabelTpes.foldRight[Tree](q"$tc.$pointMethod") {
      case ((lab, tpe), acc) =>
        val fieldTc = c.inferImplicitValue(appliedType(tcTpe, tpe), silent = true) match {
          case EmptyTree =>
            val msg = s"Cannot derive a $instanceTpe: cannot find a ${appliedType(tcTpe, tpe)} for field $lab"

            // short-circuiting the scalac logger to ensure this particular error is provided to the user
            Console.err.println(msg)
            abort(msg)

          case t => t
        }

        val args = (
          if (isLabelled)
            Seq(q"$lab")
          else
            Nil
        ) ++ Seq(
          fieldTc,
          acc
        )

        q"$tc.$prodMethod(..$args)"
    }

    q"_root_.shapeless.Derived($tc.project($t, $gen.to, $gen.from))"
  }

  def derive[T: WeakTypeTag]: Tree =
    openImplicitTpeParam match {
      case Some(tpe @ TypeRef(_, tcSym, List(paramTpe))) =>
        val tcTpe = tcSym.asType.toType.typeConstructor
        doDerive(tpe, tcTpe, paramTpe)
      case Some(tpe) =>
        abort(s"$tpe is not a type class")
      case None =>
        abort("No open implicit found")
    }

}

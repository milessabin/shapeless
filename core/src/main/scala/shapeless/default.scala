package shapeless

import scala.language.experimental.macros
import scala.reflect.macros.whitebox

/**
 * Provides default values of case class-like types.
 *
 * The `Out` type parameter is an HList type whose length is the number of fields of `T`. Its elements correspond
 * to the fields of `T`, in their original order.  It is made of `None.type` (no default value for this field) and
 * `Some[...]` (default value available for this field, with `...` the type of the field). Note that `None.type` and
 * `Some[...]` are more precise than simply `Option[...]`, so that the availability of default values can be used
 * in type level calculations.
 *
 * The `apply` method returns an HList of type `Out`, with `None` elements corresponding to no default value available,
 * and `Some(defaultValue)` to default value available for the corresponding fields.
 *
 * Use like
 * {{{
 *   case class CC(i: Int, s: String = "b")
 *
 *   val default = Default[CC]
 *
 *   // default.Out is  None.type :: Some[String] :: HNil
 *
 *   // default() returns
 *   //   None :: Some("b") :: HNil,
 *   // typed as default.Out
 * }}}
 *
 * @author Alexandre Archambault
 */
trait Default[T] extends DepFn0 with Serializable {
  type Out <: HList
}

object Default {
  def apply[T](implicit default: Default[T]): Aux[T, default.Out] = default

  def mkDefault[T, Out0 <: HList](defaults: Out0): Aux[T, Out0] =
    new Default[T] {
      type Out = Out0
      def apply() = defaults
    }

  type Aux[T, Out0 <: HList] = Default[T] { type Out = Out0 }

  implicit def materialize[T, L <: HList]: Aux[T, L] = macro DefaultMacros.materialize[T, L]
}

class DefaultMacros(val c: whitebox.Context) extends shapeless.CaseClassMacros {
  import c.universe._

  def someTpe = typeOf[Some[_]].typeConstructor
  def noneTpe = typeOf[None.type]

  def materialize[T: WeakTypeTag, L: WeakTypeTag]: Tree = {
    val tpe = weakTypeOf[T]

    if (!isCaseClassLike(classSym(tpe)))
      abort(s"$tpe is not a case class or case class like")

    lazy val companion = companionRef(tpe)

    def wrapTpeTree(idx: Int, argTpe: Type) = {
      val method = TermName(s"apply$$default$$${idx + 1}")

      tpe.companion.member(method) match {
        case NoSymbol => (noneTpe, q"_root_.scala.None")
        case defaultArg => (appliedType(someTpe, argTpe), q"_root_.scala.Some($companion.$method)")
      }
    }

    val wrapTpeTrees = fieldsOf(tpe).zipWithIndex.map {case ((_, argTpe), idx) =>
      wrapTpeTree(idx, devarargify(argTpe))
    }

    val resultTpe = mkHListTpe(wrapTpeTrees.map { case (wrapTpe, _) => wrapTpe })

    val resultTree = wrapTpeTrees.foldRight(q"_root_.shapeless.HNil": Tree) { case ((_, value), acc) =>
      q"_root_.shapeless.::($value, $acc)"
    }

    q"_root_.shapeless.Default.mkDefault[$tpe, $resultTpe]($resultTree)"
  }
}

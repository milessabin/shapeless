package shapeless

import scala.language.experimental.macros
import scala.reflect.macros.Context

import shapeless.labelled.{ FieldType, field }

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


  /**
   * Provides default values of case class-like types, as a record.
   *
   * Type `Out` is a record type, having one element per field with a default value. Labels
   * come from the available `DefaultSymbolicLabelling[T]`, and values are the default values
   * themselves.
   *
   * Method `apply` provides the record of default values, typed as `Out`.
   *
   * Example
   * {{{
   *   case class CC(i: Int, s: String = "b")
   *
   *   val default = Default.AsRecord[CC]
   *
   *   // default.Out is  Record.`'s -> String`.T
   *   // default() returns Record(s = "b")
   * }}}
   *
   * @author Alexandre Archambault
   */
  trait AsRecord[T] extends DepFn0 with Serializable {
    type Out <: HList
  }

  object AsRecord {
    def apply[T](implicit default: AsRecord[T]): Aux[T, default.Out] = default

    type Aux[T, Out0 <: HList] = AsRecord[T] { type Out = Out0 }

    trait Helper[L <: HList, Labels <: HList] extends DepFn1[L] with Serializable {
      type Out <: HList
    }

    object Helper {
      def apply[L <: HList, Labels <: HList](implicit helper: Helper[L, Labels]): Aux[L, Labels, helper.Out] = helper

      type Aux[L <: HList, Labels <: HList, Out0 <: HList] = Helper[L, Labels] { type Out = Out0 }

      implicit def hnilHelper: Aux[HNil, HNil, HNil] =
        new Helper[HNil, HNil] {
          type Out = HNil
          def apply(l: HNil) = HNil
        }

      implicit def hconsSomeHelper[K <: Symbol, H, T <: HList, LabT <: HList, OutT <: HList]
       (implicit
         tailHelper: Aux[T, LabT, OutT]
       ): Aux[Some[H] :: T, K :: LabT, FieldType[K, H] :: OutT] =
        new Helper[Some[H] :: T, K :: LabT] {
          type Out = FieldType[K, H] :: OutT
          def apply(l: Some[H] :: T) = field[K](l.head.x) :: tailHelper(l.tail)
        }

      implicit def hconsNoneHelper[K <: Symbol, T <: HList, LabT <: HList, OutT <: HList]
       (implicit
         tailHelper: Aux[T, LabT, OutT]
       ): Aux[None.type :: T, K :: LabT, OutT] =
        new Helper[None.type :: T, K :: LabT] {
          type Out = OutT
          def apply(l: None.type :: T) = tailHelper(l.tail)
        }
    }

    implicit def asRecord[T, Labels <: HList, Options <: HList, Rec <: HList]
     (implicit
       default: Default.Aux[T, Options],
       labelling: DefaultSymbolicLabelling.Aux[T, Labels],
       helper: Helper.Aux[Options, Labels, Rec]
     ): Aux[T, Rec] =
      new AsRecord[T] {
        type Out = Rec
        def apply() = helper(default())
      }
  }


  /**
   * Provides default values of case class-like types, as a HList of options.
   *
   * Unlike `Default`, `Out` is made of elements like `Option[...]` instead of `None.type` and `Some[...]`.
   * Thus, the availability of default values cannot be checked through types, only through values (via the `apply`
   * method).
   *
   * This representation can be more convenient to deal with when one only check the default values at run-time.
   *
   * Method `apply` provides the HList of default values, typed as `Out`.
   *
   * Example
   * {{{
   *   case class CC(i: Int, s: String = "b")
   *
   *   val default = Default.AsOptions[CC]
   *
   *   // default.Out is  Option[Int] :: Option[String] :: HNil
   *   // default() returns
   *   //   None :: Some("b") :: HNil
   *   // typed as default.Out
   * }}}
   *
   * @author Alexandre Archambault
   */
  trait AsOptions[T] extends DepFn0 with Serializable {
    type Out <: HList
  }

  object AsOptions {
    def apply[T](implicit default: AsOptions[T]): Aux[T, default.Out] = default

    type Aux[T, Out0 <: HList] = AsOptions[T] { type Out = Out0 }

    trait Helper[L <: HList, Repr <: HList] extends DepFn1[L] with Serializable {
      type Out <: HList
    }

    object Helper {
      def apply[L <: HList, Repr <: HList](implicit helper: Helper[L, Repr]): Aux[L, Repr, helper.Out] = helper

      type Aux[L <: HList, Repr <: HList, Out0 <: HList] = Helper[L, Repr] { type Out = Out0 }

      implicit def hnilHelper: Aux[HNil, HNil, HNil] =
        new Helper[HNil, HNil] {
          type Out = HNil
          def apply(l: HNil) = HNil
        }

      implicit def hconsSomeHelper[H, T <: HList, ReprT <: HList, OutT <: HList]
       (implicit
         tailHelper: Aux[T, ReprT, OutT]
       ): Aux[Some[H] :: T, H :: ReprT, Option[H] :: OutT] =
        new Helper[Some[H] :: T, H :: ReprT] {
          type Out = Option[H] :: OutT
          def apply(l: Some[H] :: T) = l.head :: tailHelper(l.tail)
        }

      implicit def hconsNoneHelper[H, T <: HList, ReprT <: HList, OutT <: HList]
       (implicit
         tailHelper: Aux[T, ReprT, OutT]
       ): Aux[None.type :: T, H :: ReprT, Option[H] :: OutT] =
        new Helper[None.type :: T, H :: ReprT] {
          type Out = Option[H] :: OutT
          def apply(l: None.type :: T) = None :: tailHelper(l.tail)
        }
    }

    implicit def asOption[T, Repr <: HList, Options <: HList, Out0 <: HList]
     (implicit
       default: Default.Aux[T, Options],
       gen: Generic.Aux[T, Repr],
       helper: Helper.Aux[Options, Repr, Out0]
     ): Aux[T, Out0] =
      new AsOptions[T] {
        type Out = Out0
        def apply() = helper(default())
      }
  }
}

class DefaultMacros[C <: Context](val c: C) extends CaseClassMacros {
  import c.universe._

  def someTpe = typeOf[Some[_]].typeConstructor
  def noneTpe = typeOf[None.type]

  def materialize[T: WeakTypeTag, L: WeakTypeTag]: Tree = {
    val tpe = weakTypeOf[T]

    if (!isCaseClassLike(classSym(tpe)))
      abort(s"$tpe is not a case class or case class like")

    lazy val companion = companionRef(tpe)

    // Fixes https://github.com/milessabin/shapeless/issues/474
    tpe.typeSymbol.companionSymbol.typeSignature.members

    def methodFrom(tpe: Type, name: String): Option[Symbol] = {
      val m = tpe.member(newTermName(name))
      if (m == NoSymbol)
        None
      else
        Some(m)
    }

    def wrapTpeTree(idx: Int, argTpe: Type) = {
      val methodOpt = methodFrom(tpe.typeSymbol.companionSymbol.typeSignature, s"apply$$default$$${idx + 1}")
        .orElse(methodFrom(companion.symbol.typeSignature, s"$$lessinit$$greater$$default$$${idx + 1}"))

      methodOpt match {
        case Some(method) =>
          (appliedType(someTpe, List(argTpe)), q"_root_.scala.Some($companion.$method)")
        case None =>
          (noneTpe, q"_root_.scala.None")
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

object DefaultMacros {
  def inst(c: Context) = new DefaultMacros[c.type](c)

  def materialize[T: c.WeakTypeTag, L <: HList: c.WeakTypeTag](c: Context): c.Expr[Default.Aux[T, L]] =
    c.Expr[Default.Aux[T, L]](inst(c).materialize[T, L])
}

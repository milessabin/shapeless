package shapeless

import scala.language.experimental.macros
import scala.reflect.macros.whitebox

/**
 * Evidence that no implicit `T` instance is available elsewhere.
 *
 * The instance using the `LowPriority[T]` is ignored.
 *
 * Allows to prioritize implicits, for example
 *
 * {{{
 *   trait TC[T] {
 *     def prop: Boolean
 *   }
 *
 *   object TC {
 *     // TC[Int] available by default, with field `prop` true
 *     implicit val intTC: TC[Int] = new TC[Int] { def prop = true }
 *   }
 *
 *   // extra `TC[T]`, with field `prop` false
 *   implicit def extraTC[T](implicit ev: LowPriority[TC[T]]): TC[T] =
 *     new TC[T] { def prop = false }
 *
 *   // Already available instance `intTC` is still found, because `extraTC[Int]` requires a
 *   // `LowPriority[TC[Int]]`, that will refuse to materialize (because `LowPriority` is able to
 *   // know about the already available `intTC`.)
 *   assert(implicitly[TC[Int]].prop == true)
 *
 *   // `extraTC[String]` is found, as no other `TC[String]` can be found elsewhere
 *   assert(implicitly[TC[String]].prop == false)
 * }}}
 *
 * @author Alexandre Archambault
 */
trait LowPriority[T] extends Serializable

object LowPriority {
  implicit def apply[T](implicit nf: Strict[LowPriority[T]]): LowPriority[T] =
    nf.value
}

/**
 * Allows to ignore some implicits in a `LowPriority[T]`.
 *
 * Use like ``LowPriority[Ignoring[Witness.`"ignoredMethod"`.T, T]]``.
 *
 * Typical usage is when a fallback for type class `TC` is defined in its companion, like
 * {{{
 *   object TC {
 *     implicit def anyTC[T]: TC[T] = ...
 *   }
 * }}}
 *
 * With the example of `LowPriority[T]` above,
 * {{{
 *   trait TC[T] {
 *     def prop: Option[Boolean]
 *   }
 *
 *   trait LowPriTC {
 *     // default low priority TC[T] for any T, with field `prop` equal to `None`
 *     implicit def anyTC[T]: TC[T] = new TC[T] { def prop = None }
 *   }
 *
 *   object TC extends LowPriTC {
 *     // TC[Int] available by default, with field `prop` equal to `Some(true)`
 *     implicit val intTC: TC[Int] = new TC[Int] { def prop = Some(true) }
 *   }
 *
 *   // extra `TC[T]`, with field `prop` equal to `Some(false)`
 *   implicit def extraTC[T](implicit ev: LowPriority[Ignoring[Witness.`"anyTC"`.T, TC[T]]]): TC[T] =
 *     new TC[T] { def prop = Some(false) }
 *
 *   // Already available instance `intTC` is still found, because `extraTC[Int]` requires a
 *   // `LowPriority[TC[Int]]`, that will refuse to materialize (because `LowPriority` is able to
 *   // know about the already available `intTC`.)
 *   assert(implicitly[TC[Int]].prop == true)
 *
 *   // `extraTC[String]` is found, as the default `anyTC[String]` is ignored,
 *   assert(implicitly[TC[String]].prop == false)
 * }}}
 */
trait Ignoring[M, T]

@macrocompat.bundle
trait LowPriorityTypes {
  val c: whitebox.Context

  import c.universe._


  def lowPriorityTpe: Type = typeOf[LowPriority[_]].typeConstructor

  object LowPriorityTpe {
    def unapply(tpe: Type): Option[Type] =
      tpe.dealias match {
        case TypeRef(_, cpdTpe, List(highTpe))
          if cpdTpe.asType.toType.typeConstructor =:= lowPriorityTpe =>
          Some(highTpe)
        case _ =>
          None
      }
  }

  def ignoringTpe: Type = typeOf[Ignoring[_, _]].typeConstructor

  object IgnoringTpe {
    def unapply(tpe: Type): Option[(Type, Type)] =
      tpe.dealias match {
        case TypeRef(_, cpdTpe, List(mTpe, tTpe))
          if cpdTpe.asType.toType.typeConstructor =:= ignoringTpe =>
          Some(mTpe, tTpe)
        case _ =>
          None
      }
  }

}

package shapeless
package ops

import scala.collection.generic.IsTraversableLike

object sized {
  /**
   * Type class supporting conversion of this `Sized` to an `HList` whose elements have the same type as in `Repr`.
   *
   * @author Alexandre Archambault
   */
  trait ToHList[-Repr, L <: Nat] {
    type Out <: HList
    def apply(s: Sized[Repr, L]): Out
  }

  object ToHList {
    type Aux[Repr, L <: Nat, Out0 <: HList] = ToHList[Repr, L] { type Out = Out0 }

    implicit val emptySizedToHList: Aux[Any, Nat._0, HNil] =
      new ToHList[Any, Nat._0] {
        type Out = HNil
        def apply(s: Sized[Any, Nat._0]) = HNil
      }

    implicit def nonEmptySizedToHList[Repr, L <: Nat]
      (implicit itl: IsTraversableLike[Repr], ts: ToHList[Repr, L]): Aux[Repr, Succ[L], itl.A :: ts.Out] =
        new ToHList[Repr, Succ[L]] {
          type Out = itl.A :: ts.Out
          def apply(s: Sized[Repr, Succ[L]]) = s.head :: ts(s.tail)
        }
  }

  /**
   * Type class supporting access to the ''nth'' element of this `Sized`. Available only if this `Sized` has at least
   * ''n'' elements. 
   * 
   * @author Owein Reese
   */
  trait At[Repr, L <: Nat, N <: Nat] extends DepFn1[Sized[Repr, L]]{
    type Out = IsTraversableLike[Repr]#A
  }

  object At {
    import nat._
    import LT._

    def apply[Repr, L <: Nat, N <: Nat](implicit at: At[Repr, L, N]) = at

    implicit def sizedAt1[Repr, L <: Nat](implicit itl: IsTraversableLike[Repr], ev: _0 < L) =
      new At[Repr, L, _0] {
        def apply(s: Sized[Repr, L]) = s.head
      }

    implicit def sizedAt2[Repr, L <: Nat, N <: Nat]
      (implicit at: At[Repr, L, N], itl: IsTraversableLike[Repr]) =
        new At[Repr, Succ[L], Succ[N]] {
          def apply(s: Sized[Repr, Succ[L]]) = at(s.tail)
        }
  }
}

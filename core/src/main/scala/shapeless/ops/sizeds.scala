package shapeless
package ops

import scala.collection.generic.IsTraversableLike

object sized {
  /**
   * Type class supporting conversion of this `Sized` to an `HList` whose elements have the same type as in `Repr`.
   *
   * @author Alexandre Archambault
   */
  trait ToHList[-Repr, L <: Nat] extends Serializable {
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
      (implicit itl: IsTraversableLike[Repr], ev: AdditiveCollection[Repr], ts: ToHList[Repr, L]): Aux[Repr, Succ[L], itl.A :: ts.Out] =
        new ToHList[Repr, Succ[L]] {
          type Out = itl.A :: ts.Out
          def apply(s: Sized[Repr, Succ[L]]) = s.head :: ts(s.tail)
        }
  }
}

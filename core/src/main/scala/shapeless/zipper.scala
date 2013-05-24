/*
 * Copyright (c) 2012 Miles Sabin 
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package shapeless

/**
 * Generic Zipper for any type with a representation via `Generic`.
 * 
 * @author Miles Sabin
 */
case class Zipper[C, L <: HList, R <: HList, P](prefix : L, suffix : R, parent : P) {
  import Zipper._
  import Nat._
  
  type Self = Zipper[C, L, R, P]
  
  /** Move the cursor one place to the right. Available only if not already at the rightmost element. */
  def right(implicit right : Right[Self]) : right.Out = right(this)
  
  /** Move the cursor one place to the left. Available only if not already at the leftmost element. */
  def left(implicit left : Left[Self]) : left.Out = left(this)

  /** Moves the cursor to the leftmost position. */
  def first(implicit first : First[Self]) : first.Out = first(this)
  
  /** Moves the cursor to the rightmost position. */
  def last(implicit last : Last[Self]) : last.Out = last(this)
  
  /** Move the cursor ''n'' places to the right. Requires an explicit type argument. Available only if there are
   * ''n'' places to the right of the cursor. */
  def rightBy[N <: Nat](implicit rightBy : RightBy[Self, N]) = rightBy(this)

  /** Move the cursor ''n'' places to the right. Available only if there are ''n'' places to the right of the cursor. */
  def rightBy[N <: Nat](n : N)(implicit rightBy : RightBy[Self, N]) = rightBy(this)

  /** Move the cursor ''n'' places to the left. Requires an explicit type argument. Available only if there are
   * ''n'' places to the left of the cursor. */
  def leftBy[N <: Nat](implicit leftBy : LeftBy[Self, N]) = leftBy(this)

  /** Move the cursor ''n'' places to the left. Available only if there are ''n'' places to the right of the cursor. */
  def leftBy[N <: Nat](n : N)(implicit leftBy : LeftBy[Self, N]) = leftBy(this)

  /** Move the cursor to the first element of type `T` to the right. Available only if there is an element of type `T`
   * to the right of the cursor.
   */
  def rightTo[T](implicit rightTo : RightTo[Self, T]) = rightTo(this)

  /** Move the cursor to the first element of type `T` to the left. Available only if there is an element of type `T`
   * to the left of the cursor.
   */
  def leftTo[T](implicit leftTo : LeftTo[Self, T]) = leftTo(this)
  
  /** Moves the cursor up to the next level. The element at the new cursor position will be updated with the
   * reification of the current level.
   */
  def up(implicit up : Up[Self]) : up.Out = up(this)
  
  /** Moves the cursor down to the next level, placing it at the first element on the left. Available only if the
   * element current at the cursor has a representation via `Generic`.
   */
  def down(implicit down : Down[Self]) : down.Out = down(this)
  
  /** Moves the cursor to root of this Zipper. */
  def root(implicit root : Root[Self]) : root.Out = root(this)

  /** Returns the element at the cursor. Available only if the underlying `HList` is non-empty. */
  def get(implicit get : Get[Self]) : get.Out = get(this)

  /** Replaces the element at the cursor. Available only if the underlying `HList` is non-empty. */
  def put[E](e : E)(implicit put : Put[Self, E]) : put.Out = put(this, e)

  /** Inserts a new element to the left of the cursor. */
  def insert[E](e : E)(implicit insert : Insert[Self, E]) : insert.Out = insert(this, e)

  /** Removes the element at the cursor. Available only if the underlying `HList` is non-empty. */
  def delete(implicit delete : Delete[Self]) : delete.Out = delete(this)
  
  /** Reifies the current level of this `Zipper`. */
  def reify(implicit reify : Reify[Self]) : reify.Out = reify(this)
}

object Zipper {
  def apply[C, CL <: HList](c : C)(implicit gen : GenericAux[C, CL]) : Zipper[C, HNil, CL, None.type] =
    Zipper[C, HNil, CL, None.type](HNil, gen.to(c), None)

  /** Enhances values of any type with a representation via `Generic` with a method supporting conversion to a `Zipper`. */
  class ToZipper[C](c : C) {
    def toZipper[CL <: HList](implicit gen : GenericAux[C, CL]) = Zipper(c)
  }
  
  implicit def toZipper[C](c : C) = new ToZipper(c)

  trait ZipperOp0[Z] {
    type Out
    def apply(z : Z) : Out
  }

  trait ZipperOp1[Z, T] {
    type Out
    def apply(z : Z, t : T) : Out
  }
  
  trait ZipperOp0Nat[Z, N <: Nat] {
    type Out
    def apply(z : Z) : Out
  }

  trait ZipperOp0T[Z, T] {
    type Out
    def apply(z : Z) : Out
  }

  trait Right[Z] extends ZipperOp0[Z]
  
  object Right {
    implicit def right[C, L <: HList, RH, RT <: HList, P] = new Right[Zipper[C, L, RH :: RT, P]] {
      type Out = Zipper[C, RH :: L, RT, P]
      def apply(z : Zipper[C, L, RH :: RT, P]) = Zipper(z.suffix.head :: z.prefix, z.suffix.tail, z.parent)
    }
  }
  
  trait Left[Z] extends ZipperOp0[Z]
  
  object Left {
    implicit def left[C, LH, LT <: HList, R <: HList, P] = new Left[Zipper[C, LH :: LT, R, P]] {
      type Out = Zipper[C, LT, LH :: R, P]
      def apply(z : Zipper[C, LH :: LT, R, P]) = Zipper(z.prefix.tail, z.prefix.head :: z.suffix, z.parent)
    }
  }
  
  trait First[Z] extends ZipperOp0[Z]
  
  object First {
    implicit def first[C, L <: HList, R <: HList, RP <: HList, P](implicit rp : ReversePrependAux[L, R, RP]) =
      new First[Zipper[C, L, R, P]] {
        type Out = Zipper[C, HNil, RP, P]
        def apply(z : Zipper[C, L, R, P]) = Zipper(HNil, z.prefix reverse_::: z.suffix, z.parent)
      }
  }
  
  trait Last[Z] extends ZipperOp0[Z]
  
  object Last {
    implicit def last[C, L <: HList, R <: HList, RP <: HList, P](implicit rp : ReversePrependAux[R, L, RP]) =
      new Last[Zipper[C, L, R, P]] {
        type Out = Zipper[C, RP, HNil, P]
        def apply(z : Zipper[C, L, R, P]) = Zipper(z.suffix reverse_::: z.prefix, HNil, z.parent)
      }
  }
  
  trait RightBy[Z, N <: Nat] extends ZipperOp0Nat[Z, N]
  
  object RightBy {
    import HList._
    implicit def rightBy[C, L <: HList, R <: HList, P, N <: Nat, LP <: HList, RS <: HList]
      (implicit split : SplitAux[R, N, LP, RS], reverse : ReversePrepend[LP, L]) =
        new RightBy[Zipper[C, L, R, P], N] {
          type Out = Zipper[C, reverse.Out, RS, P] 
          def apply(z : Zipper[C, L, R, P]) = {
            val (p, s) = z.suffix.split[N]
            Zipper(p reverse_::: z.prefix, s, z.parent)
          }
        }
  }

  trait LeftBy[Z, N <: Nat] extends ZipperOp0Nat[Z, N]

  object LeftBy {
    import HList._
    implicit def leftBy[C, L <: HList, R <: HList, P, N <: Nat, RP <: HList, LS <: HList]
      (implicit split : SplitAux[L, N, RP, LS], reverse : ReversePrepend[RP, R]) =
        new LeftBy[Zipper[C, L, R, P], N] {
          type Out = Zipper[C, LS, reverse.Out, P]
          def apply(z : Zipper[C, L, R, P]) = {
            val (p, s) = z.prefix.split[N]
            Zipper(s, p reverse_::: z.suffix, z.parent)
          }
        }
  }

  trait RightTo[Z, T] extends ZipperOp0T[Z, T]
  
  object RightTo {
    import HList._
    implicit def rightTo[C, L <: HList, R <: HList, P, T, LP <: HList, RS <: HList]
      (implicit split : SplitLeftAux[R, T, LP, RS], reverse : ReversePrepend[LP, L]) =
        new RightTo[Zipper[C, L, R, P], T] {
          type Out = Zipper[C, reverse.Out, RS, P]
          def apply(z : Zipper[C, L, R, P]) = {
            val (p, s) = z.suffix.splitLeft[T]
            Zipper(p reverse_::: z.prefix, s, z.parent)
          }
        }
  }

  trait LeftTo[Z, T] extends ZipperOp0T[Z, T]

  object LeftTo {
    import HList._
    implicit def leftTo[C, L <: HList, R <: HList, P, T, RP <: HList, R0 <: HList]
      (implicit split : SplitLeftAux[L, T, RP, R0], reverse : ReversePrepend[RP, R], cons : IsHCons[R0]) =
        new LeftTo[Zipper[C, L, R, P], T] {
          type Out = Zipper[C, cons.T, cons.H :: reverse.Out, P]
          def apply(z : Zipper[C, L, R, P]) = {
            val (p, s) = z.prefix.splitLeft[T]
            Zipper(s.tail, s.head :: (p reverse_::: z.suffix), z.parent)
          }
        }
  }
  
  trait Up[Z] extends ZipperOp0[Z]
  
  object Up {
    implicit def up[C, L <: HList, R <: HList, P]
      (implicit rz : Reify[Zipper[C, L, R, Some[P]]] { type Out = C }, pp : Put[P, C]) =
        new Up[Zipper[C, L, R, Some[P]]] {
          type Out = pp.Out
          def apply(z : Zipper[C, L, R, Some[P]]) = pp(z.parent.get, z.reify)
        }
  }
  
  trait Down[Z] extends ZipperOp0[Z]
  
  object Down {
    implicit def down[C, L <: HList, RH, RT <: HList, P, RHL <: HList](implicit gen : GenericAux[RH, RHL]) =
      new Down[Zipper[C, L, RH :: RT, P]] {
        type Out = Zipper[RH, HNil, RHL, Some[Zipper[C, L, RH :: RT, P]]]
        def apply(z : Zipper[C, L, RH :: RT, P]) = Zipper(HNil, gen.to(z.suffix.head), Some(z))
      }
  }
  
  trait Root[Z] extends ZipperOp0[Z]
  
  object Root extends {
    implicit def rootRoot[C, L <: HList, R <: HList] = new Root[Zipper[C, L, R, None.type]] {
      type Out = Zipper[C, L, R, None.type]
      def apply(z : Zipper[C, L, R, None.type]) = z
    }
    
    implicit def nonRootRoot[C, L <: HList, R <: HList, P, U]
      (implicit up : Up[Zipper[C, L, R, Some[P]]] { type Out = U }, pr : Root[U]) =
        new Root[Zipper[C, L, R, Some[P]]] {
          type Out = pr.Out
          def apply(z : Zipper[C, L, R, Some[P]]) = pr(z.up)
        }
  }
  
  trait Get[Z] extends ZipperOp0[Z]
  
  object Get {
    implicit def get[C, L <: HList, RH, RT <: HList, P] = new Get[Zipper[C, L, RH :: RT, P]] {
      type Out = RH
      def apply(z : Zipper[C, L, RH :: RT, P]) = z.suffix.head
    }
  }
  
  trait Put[Z, E] extends ZipperOp1[Z, E]
  
  trait LowPriorityPut {
    implicit def put[C, L <: HList, RH, RT <: HList, P, E, CL <: HList]
      (implicit gen : GenericAux[C, CL], rp : ReversePrependAux[L, E :: RT, CL]) =
        new Put[Zipper[C, L, RH :: RT, P], E] {
          type Out = Zipper[C, L, E :: RT, P]
          def apply(z : Zipper[C, L, RH :: RT, P], e : E) = Zipper(z.prefix, e :: z.suffix.tail, z.parent)
        }
  }
  
  object Put extends LowPriorityPut {
    implicit def hlistPut[C <: HList, L <: HList, RH, RT <: HList, P, E, CL <: HList]
      (implicit rp : ReversePrependAux[L, E :: RT, CL]) =
        new Put[Zipper[C, L, RH :: RT, P], E] {
          type Out = Zipper[CL, L, E :: RT, P]
          def apply(z : Zipper[C, L, RH :: RT, P], e : E) = Zipper(z.prefix, e :: z.suffix.tail, z.parent)
        }
  }
  
  trait Insert[Z, E] extends ZipperOp1[Z, E]
  
  object Insert {
    implicit def hlistInsert[C <: HList, L <: HList, R <: HList, P, E, CL <: HList]
      (implicit rp : ReversePrependAux[E :: L, R, CL]) =
        new Insert[Zipper[C, L, R, P], E] {
          type Out = Zipper[CL, E :: L, R, P]
          def apply(z : Zipper[C, L, R, P], e : E) = Zipper(e :: z.prefix, z.suffix, z.parent)
        }
  }

  trait Delete[Z] extends ZipperOp0[Z]
  
  object Delete {
    implicit def hlistDelete[C <: HList, L <: HList, RH, RT <: HList, P, CL <: HList]
      (implicit rp : ReversePrependAux[L, RT, CL]) =
        new Delete[Zipper[C, L, RH :: RT, P]] {
          type Out = Zipper[CL, L, RT, P]
          def apply(z : Zipper[C, L, RH :: RT, P]) = Zipper(z.prefix, z.suffix.tail, z.parent)
        }
  }

  trait Reify[Z] extends ZipperOp0[Z]
  
  object Reify {
    implicit def reify[C, L <: HList, R <: HList, P, CL <: HList]
      (implicit gen : GenericAux[C, CL], rp : ReversePrependAux[L, R, CL]) =
        new Reify[Zipper[C, L, R, P]] {
          type Out = C
          def apply(z : Zipper[C, L, R, P]) = gen.from(z.prefix reverse_::: z.suffix)
        }
  }
}
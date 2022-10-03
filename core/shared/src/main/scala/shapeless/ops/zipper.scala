/*
 * Copyright (c) 2012-15 Miles Sabin
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
package ops

import hlist.{ IsHCons, ReversePrepend, Split, SplitLeft }

object zipper {
  trait Right[Z] extends DepFn1[Z] with Serializable

  object Right {
    def apply[Z](implicit right: Right[Z]): Aux[Z, right.Out] = right

    type Aux[Z, Out0] = Right[Z] { type Out = Out0 }

    implicit def right[C, L <: HList, RH, RT <: HList, P]: Aux[Zipper[C, L, RH :: RT, P], Zipper[C, RH :: L, RT, P]] =
      new Right[Zipper[C, L, RH :: RT, P]] {
        type Out = Zipper[C, RH :: L, RT, P]
        def apply(z: Zipper[C, L, RH :: RT, P]) = Zipper(z.suffix.head :: z.prefix, z.suffix.tail, z.parent)
      }
  }

  trait Left[Z] extends DepFn1[Z] with Serializable

  object Left {
    def apply[Z](implicit left: Left[Z]): Aux[Z, left.Out] = left

    type Aux[Z, Out0] = Left[Z] { type Out = Out0 }

    implicit def left[C, LH, LT <: HList, R <: HList, P]: Aux[Zipper[C, LH :: LT, R, P], Zipper[C, LT, LH :: R, P]] =
      new Left[Zipper[C, LH :: LT, R, P]] {
        type Out = Zipper[C, LT, LH :: R, P]
        def apply(z: Zipper[C, LH :: LT, R, P]) = Zipper(z.prefix.tail, z.prefix.head :: z.suffix, z.parent)
      }
  }

  trait First[Z] extends DepFn1[Z] with Serializable

  object First {
    def apply[Z](implicit first: First[Z]): Aux[Z, first.Out] = first

    type Aux[Z, Out0] = First[Z] { type Out = Out0 }

    implicit def first[C, L <: HList, R <: HList, RP <: HList, P]
      (implicit rp: ReversePrepend.Aux[L, R, RP]): Aux[Zipper[C, L, R, P], Zipper[C, HNil, RP, P]] =
        new First[Zipper[C, L, R, P]] {
          type Out = Zipper[C, HNil, RP, P]
          def apply(z: Zipper[C, L, R, P]) = Zipper(HNil, z.prefix reverse_::: z.suffix, z.parent)
        }
  }

  trait Last[Z] extends DepFn1[Z] with Serializable

  object Last {
    def apply[Z](implicit last: Last[Z]): Aux[Z, last.Out] = last

    type Aux[Z, Out0] = Last[Z] { type Out = Out0 }

    implicit def last[C, L <: HList, R <: HList, RP <: HList, P]
      (implicit rp: ReversePrepend.Aux[R, L, RP]): Aux[Zipper[C, L, R, P], Zipper[C, RP, HNil, P]] =
        new Last[Zipper[C, L, R, P]] {
          type Out = Zipper[C, RP, HNil, P]
          def apply(z: Zipper[C, L, R, P]) = Zipper(z.suffix reverse_::: z.prefix, HNil, z.parent)
        }
  }

  trait RightBy[Z, N <: Nat] extends DepFn1[Z] with Serializable

  object RightBy {
    def apply[Z, N <: Nat](implicit rightBy: RightBy[Z, N]): Aux[Z, N, rightBy.Out] = rightBy

    type Aux[Z, N <: Nat, Out0] = RightBy[Z, N] { type Out = Out0 }

    implicit def rightBy[C, L <: HList, R <: HList, P, N <: Nat, LP <: HList, RS <: HList]
      (implicit
        split: Split.Aux[R, N, LP, RS],
        reverse: ReversePrepend[LP, L]): Aux[Zipper[C, L, R, P], N, Zipper[C, reverse.Out, RS, P]] =
        new RightBy[Zipper[C, L, R, P], N] {
          type Out = Zipper[C, reverse.Out, RS, P]
          def apply(z: Zipper[C, L, R, P]) = {
            val p :: s :: HNil = z.suffix.splitP[N]
            Zipper(p reverse_::: z.prefix, s, z.parent)
          }
        }
  }

  trait LeftBy[Z, N <: Nat] extends DepFn1[Z] with Serializable

  object LeftBy {
    def apply[Z, N <: Nat](implicit leftBy: LeftBy[Z, N]): Aux[Z, N, leftBy.Out] = leftBy

    type Aux[Z, N <: Nat, Out0] = LeftBy[Z, N] { type Out = Out0 }

    implicit def leftBy[C, L <: HList, R <: HList, P, N <: Nat, RP <: HList, LS <: HList]
      (implicit
        split: Split.Aux[L, N, RP, LS],
        reverse: ReversePrepend[RP, R]): Aux[Zipper[C, L, R, P], N, Zipper[C, LS, reverse.Out, P]] =
        new LeftBy[Zipper[C, L, R, P], N] {
          type Out = Zipper[C, LS, reverse.Out, P]
          def apply(z: Zipper[C, L, R, P]) = {
            val p :: s :: HNil = z.prefix.splitP[N]
            Zipper(s, p reverse_::: z.suffix, z.parent)
          }
        }
  }

  trait RightTo[Z, T] extends DepFn1[Z] with Serializable

  object RightTo {
    def apply[Z, T](implicit rightTo: RightTo[Z, T]): Aux[Z, T, rightTo.Out] = rightTo

    type Aux[Z, T, Out0] = RightTo[Z, T] { type Out = Out0 }

    implicit def rightTo[C, L <: HList, R <: HList, P, T, LP <: HList, RS <: HList]
      (implicit
        split: SplitLeft.Aux[R, T, LP, RS],
        reverse: ReversePrepend[LP, L]): Aux[Zipper[C, L, R, P], T, Zipper[C, reverse.Out, RS, P]] =
        new RightTo[Zipper[C, L, R, P], T] {
          type Out = Zipper[C, reverse.Out, RS, P]
          def apply(z: Zipper[C, L, R, P]) = {
            val p :: s :: HNil = z.suffix.splitLeftP[T]
            Zipper(p reverse_::: z.prefix, s, z.parent)
          }
        }
  }

  trait LeftTo[Z, T] extends DepFn1[Z] with Serializable

  object LeftTo {
    def apply[Z, T](implicit leftTo: LeftTo[Z, T]): Aux[Z, T, leftTo.Out] = leftTo

    type Aux[Z, T, Out0] = LeftTo[Z, T] { type Out = Out0 }

    implicit def leftTo[C, L <: HList, R <: HList, P, T, RP <: HList, R0 <: HList]
      (implicit
        split: SplitLeft.Aux[L, T, RP, R0],
        reverse: ReversePrepend[RP, R],
        cons: IsHCons[R0]): Aux[Zipper[C, L, R, P], T, Zipper[C, cons.T, cons.H :: reverse.Out, P]] =
        new LeftTo[Zipper[C, L, R, P], T] {
          type Out = Zipper[C, cons.T, cons.H :: reverse.Out, P]
          def apply(z: Zipper[C, L, R, P]) = {
            val p :: s :: HNil = z.prefix.splitLeftP[T]
            Zipper(s.tail, s.head :: (p reverse_::: z.suffix), z.parent)
          }
        }
  }

  trait Up[Z] extends DepFn1[Z] with Serializable

  object Up {
    def apply[Z](implicit up: Up[Z]): Aux[Z, up.Out] = up

    type Aux[Z, Out0] = Up[Z] { type Out = Out0 }

    implicit def up[C, L <: HList, R <: HList, P]
      (implicit
        rz: Reify.Aux[Zipper[C, L, R, Some[P]], C],
        pp: Put[P, C]): Aux[Zipper[C, L, R, Some[P]], pp.Out] =
        new Up[Zipper[C, L, R, Some[P]]] {
          type Out = pp.Out
          def apply(z: Zipper[C, L, R, Some[P]]) = pp(z.parent.get, z.reify)
        }
  }

  trait Down[Z] extends DepFn1[Z] with Serializable

  object Down {
    def apply[Z](implicit down: Down[Z]): Aux[Z, down.Out] = down

    type Aux[Z, Out0] = Down[Z] { type Out = Out0 }

    implicit def hlistDown[C, L <: HList, RH <: HList, RT <: HList, P]:
      Aux[Zipper[C, L, RH :: RT, P], Zipper[RH, HNil, RH, Some[Zipper[C, L, RH :: RT, P]]]] =
        new Down[Zipper[C, L, RH :: RT, P]] {
          type Out = Zipper[RH, HNil, RH, Some[Zipper[C, L, RH :: RT, P]]]
          def apply(z: Zipper[C, L, RH :: RT, P]) = Zipper(HNil, z.suffix.head, Some(z))
        }

    implicit def genericDown[C, L <: HList, RH, RT <: HList, P, RHL <: HList](implicit gen: Generic.Aux[RH, RHL]):
      Aux[Zipper[C, L, RH :: RT, P], Zipper[RH, HNil, RHL, Some[Zipper[C, L, RH :: RT, P]]]] =
        new Down[Zipper[C, L, RH :: RT, P]] {
          type Out = Zipper[RH, HNil, RHL, Some[Zipper[C, L, RH :: RT, P]]]
          def apply(z: Zipper[C, L, RH :: RT, P]) = Zipper(HNil, gen.to(z.suffix.head), Some(z))
        }
  }

  trait Root[Z] extends DepFn1[Z] with Serializable

  object Root extends {
    def apply[Z](implicit root: Root[Z]): Aux[Z, root.Out] = root

    type Aux[Z, Out0] = Root[Z] { type Out = Out0 }

    implicit def rootRoot[C, L <: HList, R <: HList]: Aux[Zipper[C, L, R, None.type], Zipper[C, L, R, None.type]] =
      new Root[Zipper[C, L, R, None.type]] {
        type Out = Zipper[C, L, R, None.type]
        def apply(z: Zipper[C, L, R, None.type]) = z
      }

    implicit def nonRootRoot[C, L <: HList, R <: HList, P, U]
      (implicit
        up: Up.Aux[Zipper[C, L, R, Some[P]], U],
        pr: Root[U]): Aux[Zipper[C, L, R, Some[P]], pr.Out] =
        new Root[Zipper[C, L, R, Some[P]]] {
          type Out = pr.Out
          def apply(z: Zipper[C, L, R, Some[P]]) = pr(z.up)
        }
  }

  trait Get[Z] extends DepFn1[Z] with Serializable

  object Get {
    def apply[Z](implicit get: Get[Z]): Aux[Z, get.Out] = get

    type Aux[Z, Out0] = Get[Z] { type Out = Out0 }

    implicit def get[C, L <: HList, RH, RT <: HList, P]: Aux[Zipper[C, L, RH :: RT, P], RH] =
      new Get[Zipper[C, L, RH :: RT, P]] {
        type Out = RH
        def apply(z: Zipper[C, L, RH :: RT, P]) = z.suffix.head
      }
  }

  trait Put[Z, E] extends DepFn2[Z, E] with Serializable

  object Put {
    def apply[Z, E](implicit put: Put[Z, E]): Aux[Z, E, put.Out] = put

    type Aux[Z, E, Out0] = Put[Z, E] { type Out = Out0 }

    implicit def genericPut[C, L <: HList, RH, RT <: HList, P, E, CL <: HList]
      (implicit
        gen: Generic.Aux[C, CL],
        rp: ReversePrepend.Aux[L, E :: RT, CL]): Aux[Zipper[C, L, RH :: RT, P], E, Zipper[C, L, E :: RT, P]] =
        new Put[Zipper[C, L, RH :: RT, P], E] {
          type Out = Zipper[C, L, E :: RT, P]
          def apply(z: Zipper[C, L, RH :: RT, P], e: E) = Zipper(z.prefix, e :: z.suffix.tail, z.parent)
        }

    implicit def hlistPut[C <: HList, L <: HList, RH, RT <: HList, P, E, CL <: HList]
      (implicit rp: ReversePrepend.Aux[L, E :: RT, CL]): Aux[Zipper[C, L, RH :: RT, P], E, Zipper[CL, L, E :: RT, P]] =
        new Put[Zipper[C, L, RH :: RT, P], E] {
          type Out = Zipper[CL, L, E :: RT, P]
          def apply(z: Zipper[C, L, RH :: RT, P], e: E) = Zipper(z.prefix, e :: z.suffix.tail, z.parent)
        }
  }

  trait Modify[Z, E1, E2] extends DepFn2[Z, E1 => E2] with Serializable

  object Modify {
    def apply[Z, E1, E2](implicit modify: Modify[Z, E1, E2]): Aux[Z, E1, E2, modify.Out] = modify

    type Aux[Z, E1, E2, Out0] = Modify[Z, E1, E2] { type Out = Out0 }

    implicit def modify[C, L <: HList, RH1, RT <: HList, P, RH2]
      (implicit
        get: Get.Aux[Zipper[C, L, RH1 :: RT, P], RH1],
        put: Put.Aux[Zipper[C, L, RH1 :: RT, P], RH2, Zipper[C, L, RH2 :: RT, P]]
      ): Aux[Zipper[C, L, RH1 :: RT, P], RH1, RH2, Zipper[C, L, RH2 :: RT, P]] =
        new Modify[Zipper[C, L, RH1 :: RT, P], RH1, RH2] {
          type Out = Zipper[C, L, RH2 :: RT, P]
          def apply(z: Zipper[C, L, RH1 :: RT, P], f: RH1 => RH2) = put(z, f(get(z)))
        }
  }

  trait Insert[Z, E] extends DepFn2[Z, E] with Serializable

  object Insert {
    def apply[Z, E](implicit insert: Insert[Z, E]): Aux[Z, E, insert.Out] = insert

    type Aux[Z, E, Out0] = Insert[Z, E] { type Out = Out0 }

    implicit def hlistInsert[C <: HList, L <: HList, R <: HList, P, E, CL <: HList]
      (implicit rp: ReversePrepend.Aux[E :: L, R, CL]): Aux[Zipper[C, L, R, P], E, Zipper[CL, E :: L, R, P]] =
        new Insert[Zipper[C, L, R, P], E] {
          type Out = Zipper[CL, E :: L, R, P]
          def apply(z: Zipper[C, L, R, P], e: E) = Zipper(e :: z.prefix, z.suffix, z.parent)
        }
  }

  trait Delete[Z] extends DepFn1[Z] with Serializable

  object Delete {
    def apply[Z](implicit delete: Delete[Z]): Aux[Z, delete.Out] = delete

    type Aux[Z, Out0] = Delete[Z] { type Out = Out0 }

    implicit def hlistDelete[C <: HList, L <: HList, RH, RT <: HList, P, CL <: HList]
      (implicit rp: ReversePrepend.Aux[L, RT, CL]): Aux[Zipper[C, L, RH :: RT, P], Zipper[CL, L, RT, P]] =
        new Delete[Zipper[C, L, RH :: RT, P]] {
          type Out = Zipper[CL, L, RT, P]
          def apply(z: Zipper[C, L, RH :: RT, P]) = Zipper(z.prefix, z.suffix.tail, z.parent)
        }
  }

  trait Reify[Z] extends DepFn1[Z] with Serializable

  object Reify {
    def apply[Z](implicit reify: Reify[Z]): Aux[Z, reify.Out] = reify

    type Aux[Z, Out0] = Reify[Z] { type Out = Out0 }

    implicit def hlistReify[LR <: HList, L <: HList, R <: HList, P]
      (implicit rp: ReversePrepend.Aux[L, R, LR]): Aux[Zipper[LR, L, R, P], LR] =
        new Reify[Zipper[LR, L, R, P]] {
          type Out = LR
          def apply(z: Zipper[LR, L, R, P]) = z.prefix reverse_::: z.suffix
        }

    implicit def genericReify[C, L <: HList, R <: HList, P, CL <: HList]
      (implicit
        gen: Generic.Aux[C, CL],
        rp: ReversePrepend.Aux[L, R, CL]): Aux[Zipper[C, L, R, P], C] =
        new Reify[Zipper[C, L, R, P]] {
          type Out = C
          def apply(z: Zipper[C, L, R, P]) = gen.from(z.prefix reverse_::: z.suffix)
        }
  }
}

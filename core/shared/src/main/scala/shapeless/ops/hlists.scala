/*
 * Copyright (c) 2011-18 Miles Sabin
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

import scala.annotation.implicitNotFound
import scala.collection._
import scala.collection.mutable

import poly._

object hlist {
  /**
   * Type class witnessing that this `HList` is composite and providing access to head and tail.
   *
   * @author Miles Sabin
   */
  trait IsHCons[L <: HList] extends Serializable {
    type H
    type T <: HList

    def head(l : L) : H
    def tail(l : L) : T
    def cons(h : H, t : T) : L
  }

  object IsHCons {
    def apply[L <: HList](implicit isHCons: IsHCons[L]): Aux[L, isHCons.H, isHCons.T] = isHCons

    type Aux[L <: HList, H0, T0 <: HList] = IsHCons[L] { type H = H0; type T = T0 }
    implicit def hlistIsHCons[H0, T0 <: HList]: Aux[H0 :: T0, H0, T0] =
      new IsHCons[H0 :: T0] {
        type H = H0
        type T = T0

        def head(l : H0 :: T0) : H = l.head
        def tail(l : H0 :: T0) : T = l.tail
        def cons(h : H0, t : T0) : H0 :: T0 = h :: t
      }
  }

  /**
   * Type class witnessing that the result of wrapping each element of `HList` `L` in type constructor `F` is `Out`.
   */
  trait Mapped[L <: HList, F[_]] extends Serializable {
    type Out <: HList
  }

  trait LowPriorityMapped {
    type Aux[L <: HList, F[_], Out0 <: HList] =
      Mapped[L, F] { type Out = Out0 }

    implicit def hlistIdMapped[H, T <: HList]: Aux[H :: T, Id, H :: T] =
      new Mapped[H :: T, Id] { type Out = H :: T }

    implicit def hlistMapped2[H, T <: HList, F, OutM <: HList](
      implicit mt: Mapped.Aux[T, Const[F]#λ, OutM]
    ): Aux[H :: T, Const[F]#λ, F :: OutM] =
      new Mapped[H :: T, Const[F]#λ] { type Out = F :: OutM }
  }

  object Mapped extends LowPriorityMapped {
    def apply[L <: HList, F[_]](implicit mapped: Mapped[L, F]): Aux[L, F, mapped.Out] = mapped

    implicit def hnilMapped[F[_]]: Aux[HNil, F, HNil] = new Mapped[HNil, F] { type Out = HNil }

    implicit def hnilConstMapped[F]: Aux[HNil, Const[F]#λ, HNil] =
      new Mapped[HNil, Const[F]#λ] { type Out = HNil }

    implicit def hlistMapped1[H, T <: HList, F[_], OutM <: HList](implicit mt : Mapped.Aux[T, F, OutM]): Aux[H :: T, F, F[H] :: OutM] =
      new Mapped[H :: T, F] { type Out = F[H] :: OutM }
  }

  /**
   * Type class witnessing that the result of stripping type constructor `F` off each element of `HList` `L` is `Out`.
   */
  trait Comapped[L <: HList, F[_]] extends Serializable {
    type Out <: HList
  }

  trait LowPriorityComapped {
    type Aux[L <: HList, F[_], Out0 <: HList] = Comapped[L, F] { type Out = Out0 }
    implicit def hlistIdComapped[L <: HList]: Aux[L, Id, L] = new Comapped[L, Id] { type Out = L }
  }

  object Comapped extends LowPriorityComapped {
    def apply[L <: HList, F[_]](implicit comapped: Comapped[L, F]): Aux[L, F, comapped.Out] = comapped

    implicit def hnilComapped[F[_]]: Aux[HNil, F, HNil] = new Comapped[HNil, F] { type Out = HNil }

    implicit def hlistComapped[H, T <: HList, F[_], TCM <: HList](
      implicit mt : Comapped.Aux[T, F, TCM]
    ): Aux[F[H] :: T, F, H :: TCM] =
      new Comapped[F[H] :: T, F] { type Out = H :: TCM }
  }

  /**
   * Type class witnessing that `HList`s `L1` and `L2` have elements of the form `F1[Ln]` and `F2[Ln]` respectively for all
   * indices `n`. This implies that a natural transform `F1 ~> F2` will take a list of type `L1` onto a list of type `L2`.
   *
   * @author Miles Sabin
   */
  trait NatTRel[L1 <: HList, F1[_], L2 <: HList, F2[_]] extends Serializable {
    def map(nt: F1 ~> F2, fa: L1): L2
  }

  object NatTRel {
    def apply[L1 <: HList, F1[_], L2 <: HList, F2[_]](implicit natTRel: NatTRel[L1, F1, L2, F2]) = natTRel

    implicit def hnilNatTRel1[F1[_], F2[_]] = new NatTRel[HNil, F1, HNil, F2] {
      def map(f: F1 ~> F2, fa: HNil): HNil = HNil
    }

    implicit def hnilNatTRel2[F1[_], H2] = new NatTRel[HNil, F1, HNil, Const[H2]#λ] {
      def map(f: F1 ~> Const[H2]#λ, fa: HNil): HNil = HNil
    }

    implicit def hlistNatTRel1[H, F1[_], F2[_], T1 <: HList, T2 <: HList](implicit nt : NatTRel[T1, F1, T2, F2]) =
      new NatTRel[F1[H] :: T1, F1, F2[H] :: T2, F2] {
        def map(f: F1 ~> F2, fa: F1[H] :: T1): F2[H] :: T2 = f(fa.head) :: nt.map(f, fa.tail)
      }

    implicit def hlistNatTRel2[H, F2[_], T1 <: HList, T2 <: HList](implicit nt : NatTRel[T1, Id, T2, F2]) =
      new NatTRel[H :: T1, Id, F2[H] :: T2, F2] {
        def map(f: Id ~> F2, fa: H :: T1): F2[H] :: T2 = f(fa.head) :: nt.map(f, fa.tail)
      }

    implicit def hlistNatTRel3[H, F1[_], T1 <: HList, T2 <: HList](implicit nt : NatTRel[T1, F1, T2, Id]) =
      new NatTRel[F1[H] :: T1, F1, H :: T2, Id] {
        def map(f: F1 ~> Id, fa: F1[H] :: T1): H :: T2 = f(fa.head) :: nt.map(f, fa.tail)
      }

    implicit def hlistNatTRel4[H1, F1[_], T1 <: HList, H2, T2 <: HList](implicit nt : NatTRel[T1, F1, T2, Const[H2]#λ]) =
      new NatTRel[F1[H1] :: T1, F1, H2 :: T2, Const[H2]#λ] {
        def map(f: F1 ~> Const[H2]#λ, fa: F1[H1] :: T1): H2 :: T2 = f(fa.head) :: nt.map(f, fa.tail)
      }

    implicit def hlistNatTRel5[H1, T1 <: HList, H2, T2 <: HList](implicit nt : NatTRel[T1, Id, T2, Const[H2]#λ]) =
      new NatTRel[H1 :: T1, Id, H2 :: T2, Const[H2]#λ] {
        def map(f: Id ~> Const[H2]#λ, fa: H1 :: T1): H2 :: T2 = f(fa.head) :: nt.map(f, fa.tail)
      }
  }

  /**
   * Type class providing minimally witnessed operations on `HList`s which can be derived from `L` by wrapping
   * each of its elements in a type constructor.
   */
  trait HKernel {
    type L <: HList
    type Mapped[G[_]] <: HList
    type Length <: Nat

    def map[F[_], G[_]](f: F ~> G, l: Mapped[F]): Mapped[G]

    def tabulate[C](from: Int)(f: Int => C): Mapped[Const[C]#λ]

    def toList[C](l: Mapped[Const[C]#λ]): List[C]

    def length: Int
  }

  trait HNilHKernel extends HKernel {
    type L = HNil
    type Mapped[G[_]] = HNil
    type Length = _0

    def map[F[_], G[_]](f: F ~> G, l: HNil): HNil = HNil

    def tabulate[C](from: Int)(f: Int => C): HNil = HNil

    def toList[C](l: HNil): List[C] = Nil

    def length: Int = 0
  }

  case object HNilHKernel extends HNilHKernel

  final case class HConsHKernel[H, T <: HKernel](tail: T) extends HKernel {
    type L = H :: tail.L
    type Mapped[G[_]] = G[H] :: tail.Mapped[G]
    type Length = Succ[tail.Length]

    def map[F[_], G[_]](f: F ~> G, l: F[H] :: tail.Mapped[F]): G[H] :: tail.Mapped[G] = f(l.head) :: tail.map(f, l.tail)

    def tabulate[C](from: Int)(f: Int => C): C :: tail.Mapped[Const[C]#λ] = f(from) :: tail.tabulate(from+1)(f)

    def toList[C](l: C :: tail.Mapped[Const[C]#λ]): List[C] = l.head :: tail.toList(l.tail)

    def length: Int = 1+tail.length
  }

  object HKernel {
    def apply[L <: HList](implicit mk: HKernelAux[L]): mk.Out = mk()
    def apply[L <: HList](l: L)(implicit mk: HKernelAux[L]): mk.Out = mk()
  }

  trait HKernelAux[L <: HList] extends DepFn0 { type Out <: HKernel }

  object HKernelAux {

    type Aux[L <: HList, Out0 <: HKernel] = HKernelAux[L] { type Out = Out0 }

    implicit def mkHNilHKernel = new HKernelAux[HNil] {
      type Out = HNilHKernel
      def apply() = HNilHKernel
    }

    implicit def mkHListHKernel[H, T <: HList, CtOut <: HKernel](implicit ct: HKernelAux.Aux[T, CtOut]) = new HKernelAux[H :: T] {
      type Out = HConsHKernel[H, CtOut]
      def apply() = HConsHKernel[H, CtOut](ct())
    }
  }

  /**
   * Type class computing the coproduct type corresponding to this `HList`.
   *
   * @author Miles Sabin
   */
  trait ToCoproduct[L <: HList] extends Serializable { type Out <: Coproduct }

  object ToCoproduct {
    def apply[L <: HList](implicit tcp: ToCoproduct[L]): Aux[L, tcp.Out] = tcp

    type Aux[L <: HList, Out0 <: Coproduct] = ToCoproduct[L] { type Out = Out0 }

    implicit val hnilToCoproduct: Aux[HNil, CNil] =
      new ToCoproduct[HNil] {
        type Out = CNil
      }

    implicit def hlistToCoproduct[H, T <: HList, UtOut <: Coproduct](implicit ut: ToCoproduct.Aux[T, UtOut]): Aux[H :: T, H :+: UtOut] =
      new ToCoproduct[H :: T] {
        type Out = H :+: UtOut
      }
  }

  /**
   * Type class computing the sum type corresponding to this `HList`.
   *
   */
  trait lowPriorityToSum {
    implicit def hlistToSum[H, T <: HList, UtOut <: Coproduct](implicit ut: ToSum.Aux[T, UtOut]): ToSum.Aux[H :: T, H :+: UtOut] =
      new ToSum[H :: T] {
        type Out = H :+: UtOut
      }
  }

  trait ToSum[L <: HList] extends Serializable {
    type Out <: Coproduct
  }

  object ToSum extends lowPriorityToSum {
    def apply[L <: HList](implicit tcp: ToSum[L]): Aux[L, tcp.Out] = tcp

    type Aux[L <: HList, Out0 <: Coproduct] = ToSum[L] {type Out = Out0}

    implicit val hnilToSum: Aux[HNil, CNil] =
      new ToSum[HNil] {
        type Out = CNil
      }

    implicit def hlistToSum[H, T <: HList, OutL <: HList, OutS <: Coproduct](implicit
                                                          fn: FilterNot.Aux[T, H, OutL],
                                                          ut: ToSum.Aux[OutL, OutS]): Aux[H :: T, H :+: OutS] =
      new ToSum[H :: T] {
        type Out = H :+: OutS
      }
  }

  /**
   * Type class supporting computing the type-level Nat corresponding to the length of this `HList`.
   *
   * @author Miles Sabin
   */
  trait Length[L <: HList] extends DepFn0 with Serializable { type Out <: Nat }

  object Length {
    def apply[L <: HList](implicit length: Length[L]): Aux[L, length.Out] = length

    import shapeless.nat._
    type Aux[L <: HList, N <: Nat] = Length[L] { type Out = N }
    implicit def hnilLength[L <: HNil]: Aux[L, _0] = new Length[L] {
      type Out = _0
      def apply(): Out = _0
    }

    implicit def hlistLength[H, T <: HList, N <: Nat](implicit lt : Aux[T, N], sn : Witness.Aux[Succ[N]]): Aux[H :: T, Succ[N]] = new Length[H :: T] {
      type Out = Succ[N]
      def apply(): Out = sn.value
    }
  }

  /**
   * Type class supporting mapping a higher ranked function over this `HList`.
   *
   * @author Miles Sabin
   */
  trait Mapper[HF, In <: HList] extends DepFn1[In] with Serializable { type Out <: HList }

  object Mapper {
    def apply[F, L <: HList](implicit mapper: Mapper[F, L]): Aux[F, L, mapper.Out] = mapper

    type Aux[HF, In <: HList, Out0 <: HList] = Mapper[HF, In] { type Out = Out0 }

    implicit def hnilMapper1[HF]: Aux[HF, HNil, HNil] =
      new Mapper[HF, HNil] {
        type Out = HNil
        def apply(l : HNil): Out = HNil
      }

    implicit def hlistMapper1[HF <: Poly, InH, InT <: HList, OutT <: HList]
      (implicit hc : Case1[HF, InH], mt : Mapper.Aux[HF, InT, OutT]): Aux[HF, InH :: InT, hc.Result :: OutT] =
        new Mapper[HF, InH :: InT] {
          type Out = hc.Result :: OutT
          def apply(l : InH :: InT): Out = hc(l.head) :: mt(l.tail)
        }
  }

  /**
   * Type class supporting flatmapping a higher ranked function over this `HList`.
   *
   * @author Miles Sabin
   */
  trait FlatMapper[HF, In <: HList] extends DepFn1[In] with Serializable { type Out <: HList }

  object FlatMapper {
    def apply[F, L <: HList](implicit mapper: FlatMapper[F, L]): Aux[F, L, mapper.Out] = mapper

    type Aux[HF, In <: HList, Out0 <: HList] = FlatMapper[HF, In] { type Out = Out0 }

    implicit def hnilFlatMapper1[HF]: Aux[HF, HNil, HNil] =
      new FlatMapper[HF, HNil] {
        type Out = HNil
        def apply(l : HNil): Out = HNil
      }

    implicit def hlistFlatMapper1[HF <: Poly, InH, OutH <: HList, InT <: HList, OutT <: HList, Out0 <: HList]
      (implicit
        hc : Case1.Aux[HF, InH, OutH],
        mt : FlatMapper.Aux[HF, InT, OutT],
        prepend : Prepend.Aux[OutH, OutT, Out0]
      ): Aux[HF, InH :: InT, Out0] =
        new FlatMapper[HF, InH :: InT] {
          type Out = Out0
          def apply(l : InH :: InT): Out = prepend(hc(l.head), mt(l.tail))
        }
  }

  /**
   * Type class supporting mapping a constant valued function over this `HList`.
   *
   * @author Miles Sabin
   */
  trait ConstMapper[C, L <: HList] extends DepFn2[C, L] with Serializable { type Out <: HList }

  object ConstMapper {
    def apply[C, L <: HList](implicit mapper: ConstMapper[C, L]): Aux[C, L, mapper.Out] = mapper

    type Aux[C, L <: HList, Out0 <: HList] = ConstMapper[C, L] { type Out = Out0 }

    implicit def hnilConstMapper[C]: Aux[C, HNil, HNil] =
      new ConstMapper[C, HNil] {
        type Out = HNil
        def apply(c : C, l : HNil): Out = l
      }

    implicit def hlistConstMapper[H, T <: HList, C, OutT <: HList]
    (implicit mct : ConstMapper.Aux[C, T, OutT]): Aux[C, H :: T, C :: OutT] =
        new ConstMapper[C, H :: T] {
          type Out = C :: OutT
          def apply(c : C, l : H :: T): Out = c :: mct(c, l.tail)
        }
  }

  /**
   * Type class supporting mapping a polymorphic function over this `HList` and then folding the result using a
   * monomorphic function value.
   *
   * @author Miles Sabin
   */
  trait MapFolder[L <: HList, R, HF] extends Serializable {
    def apply(l : L, in : R, op : (R, R) => R) : R
  }

  object MapFolder {
    def apply[L <: HList, R, F](implicit folder: MapFolder[L, R, F]): MapFolder[L, R, F] = folder

    implicit def hnilMapFolder[R, HF]: MapFolder[HNil, R, HF] = new MapFolder[HNil, R, HF] {
      def apply(l : HNil, in : R, op : (R, R) => R): R = in
    }

    implicit def hlistMapFolder[H, T <: HList, R, HF <: Poly]
      (implicit hc : Case1.Aux[HF, H, R], tf : MapFolder[T, R, HF]): MapFolder[H :: T, R, HF] =
        new MapFolder[H :: T, R, HF] {
          def apply(l : H :: T, in : R, op : (R, R) => R): R = op(hc(l.head), tf(l.tail, in, op))
        }
  }

  /**
   * Type class supporting left-folding a polymorphic binary function over this `HList`.
   *
   * @author Miles Sabin
   */
  trait LeftFolder[L <: HList, In, HF] extends DepFn2[L, In] with Serializable

  object LeftFolder {
    def apply[L <: HList, In, F](implicit folder: LeftFolder[L, In, F]): Aux[L, In, F, folder.Out] = folder

    type Aux[L <: HList, In, HF, Out0] = LeftFolder[L, In, HF] { type Out = Out0 }

    implicit def hnilLeftFolder[In, HF]: Aux[HNil, In , HF, In] =
      new LeftFolder[HNil, In, HF] {
        type Out = In
        def apply(l : HNil, in : In): Out = in
      }

    implicit def hlistLeftFolder[H, T <: HList, In, HF, OutH, FtOut]
      (implicit f : Case2.Aux[HF, In, H, OutH], ft : LeftFolder.Aux[T, OutH, HF, FtOut]): Aux[H :: T, In, HF, FtOut] =
        new LeftFolder[H :: T, In, HF] {
          type Out = FtOut
          def apply(l : H :: T, in : In) : Out = ft(l.tail, f(in, l.head))
        }
  }

  /**
   * Type class supporting right-folding a polymorphic binary function over this `HList`.
   *
   * @author Miles Sabin
   */
  trait RightFolder[L <: HList, In, HF] extends DepFn2[L, In] with Serializable

  object RightFolder {
    def apply[L <: HList, In, F](implicit folder: RightFolder[L, In, F]): Aux[L, In, F, folder.Out] = folder

    type Aux[L <: HList, In, HF, Out0] = RightFolder[L, In, HF] { type Out = Out0 }

    implicit def hnilRightFolder[In, HF]: Aux[HNil, In, HF, In] =
      new RightFolder[HNil, In, HF] {
        type Out = In
        def apply(l : HNil, in : In): Out = in
      }

    implicit def hlistRightFolder[H, T <: HList, In, HF, OutT]
      (implicit ft : RightFolder.Aux[T, In, HF, OutT], f : Case2[HF, H, OutT]): Aux[H :: T, In, HF, f.Result] =
        new RightFolder[H :: T, In, HF] {
          type Out = f.Result
          def apply(l : H :: T, in : In): Out = f(l.head, ft(l.tail, in))
        }
  }

  /**
   * Type class supporting left-reducing a polymorphic binary function over this `HList`.
   *
   * @author Miles Sabin
   */
  trait LeftReducer[L <: HList, HF] extends DepFn1[L] with Serializable

  object LeftReducer {
    def apply[L <: HList, F](implicit reducer: LeftReducer[L, F]): Aux[L, F, reducer.Out] = reducer

    type Aux[L <: HList, HF, Out0] = LeftReducer[L, HF] { type Out = Out0 }
    implicit def leftReducer[H, T <: HList, HF, FolderOut](implicit folder : LeftFolder.Aux[T, H, HF, FolderOut]): Aux[H :: T, HF, FolderOut] =
      new LeftReducer[H :: T, HF] {
        type Out = FolderOut
        def apply(l : H :: T) : Out = folder.apply(l.tail, l.head)
      }
  }

  /**
   * Type class supporting right-reducing a polymorphic binary function over this `HList`.
   *
   * @author Miles Sabin
   */
  trait RightReducer[L <: HList, HF] extends DepFn1[L] with Serializable

  object RightReducer {
    def apply[L <: HList, F](implicit reducer: RightReducer[L, F]): Aux[L, F, reducer.Out] = reducer

    type Aux[L <: HList, HF, Out0] = RightReducer[L, HF] { type Out = Out0 }

    implicit def hsingleRightReducer[H, HF]: Aux[H :: HNil, HF, H] =
      new RightReducer[H :: HNil, HF] {
        type Out = H
        def apply(l : H :: HNil): Out = l.head
      }

    implicit def hlistRightReducer[H, T <: HList, HF, OutT]
      (implicit rt : RightReducer.Aux[T, HF, OutT], f : Case2[HF, H, OutT]): Aux[H :: T, HF, f.Result] =
        new RightReducer[H :: T, HF] {
          type Out = f.Result
          def apply(l : H :: T): Out = f(l.head, rt(l.tail))
        }
  }

  /**
   * Type class supporting unification of this `HList`.
   *
   * @author Miles Sabin
   */
  trait Unifier[L <: HList] extends DepFn1[L] with Serializable { type Out <: HList }

  object Unifier {
    def apply[L <: HList](implicit unifier: Unifier[L]): Aux[L, unifier.Out] = unifier

    type Aux[L <: HList, Out0 <: HList] = Unifier[L] { type Out = Out0 }

    implicit val hnilUnifier: Aux[HNil, HNil] = new Unifier[HNil] {
      type Out = HNil
      def apply(l : HNil): Out = l
    }

    implicit def hsingleUnifier[T]: Aux[T :: HNil, T :: HNil] =
      new Unifier[T :: HNil] {
        type Out = T :: HNil
        def apply(l : T :: HNil): Out = l
      }

    implicit def hlistUnifier[H1, H2, L, T <: HList, LtOut <: HList]
      (implicit u : Lub[H1, H2, L], lt : Unifier.Aux[L :: T, LtOut]): Aux[H1 :: H2 :: T, L :: LtOut] =
        new Unifier[H1 :: H2 :: T] {
          type Out = L :: LtOut
          def apply(l : H1 :: H2 :: T): Out = u.left(l.head) :: lt(u.right(l.tail.head) :: l.tail.tail)
        }
  }

  /**
   * Type class supporting unification of all elements that are subtypes of `B` in this `HList` to `B`, with all other
   * elements left unchanged.
   *
   * @author Travis Brown
   */
  trait SubtypeUnifier[L <: HList, B] extends DepFn1[L] with Serializable { type Out <: HList }

  object SubtypeUnifier {
    def apply[L <: HList, B](implicit unifier: SubtypeUnifier[L, B]): Aux[L, B, unifier.Out] = unifier

    type Aux[L <: HList, B, Out0 <: HList] = SubtypeUnifier[L, B] { type Out = Out0 }

    implicit def hnilSubtypeUnifier[B]: Aux[HNil, B, HNil] =
      new SubtypeUnifier[HNil, B] {
        type Out = HNil
        def apply(l : HNil): Out = l
      }

    implicit def hlistSubtypeUnifier1[H, T <: HList, B, SutOut <: HList]
      (implicit st : H <:< B, sut: SubtypeUnifier.Aux[T, B, SutOut]): Aux[H :: T, B, B :: SutOut] =
        new SubtypeUnifier[H :: T, B] {
          type Out = B :: SutOut
          def apply(l : H :: T): Out = st(l.head) :: sut(l.tail)
        }

    implicit def hlistSubtypeUnifier2[H, T <: HList, B, SutOut <: HList]
      (implicit nst : H <:!< B, sut: SubtypeUnifier.Aux[T, B, SutOut]): Aux[H :: T, B, H :: SutOut] =
        new SubtypeUnifier[H :: T, B] {
          type Out = H :: SutOut
          def apply(l : H :: T): Out = l.head :: sut(l.tail)
        }
  }

  /**
   * Type class supporting conversion of this `HList` to a `M` with elements typed
   * as the least upper bound Lub of the types of the elements of this `HList`.
   *
   * Serializable if the `Factory`s it implicitly finds are too.
   * Note that the `Factory`s from the standard library are *not*
   * serializable. See the tests for how to make your own serializable
   * `Factory` available to `ToTraversable`.
   *
   * @author Alexandre Archambault
   */
  trait ToTraversable[L <: HList, M[_]] extends DepFn1[L] with Serializable {
    type Lub
    def builder(): mutable.Builder[Lub, M[Lub]]
    def append[LLub](l: L, b: mutable.Builder[LLub, M[LLub]], f: Lub => LLub): Unit

    type Out = M[Lub]
    def apply(l: L): Out = {
      val b = builder()
      append(l, b, identity)
      b.result()
    }
  }

  object ToTraversable {
    def apply[L <: HList, M[_]]
      (implicit toTraversable: ToTraversable[L, M]): Aux[L, M, toTraversable.Lub] = toTraversable

    type Aux[L <: HList, M[_], Lub0] = ToTraversable[L, M] { type Lub = Lub0 }

    implicit def hnilToTraversable[L <: HNil, M[_], T]
      (implicit cbf : Factory[T, M[T]]) : Aux[L, M, T] =
        new ToTraversable[L, M] {
          type Lub = T
          def builder() = cbf.newBuilder
          def append[LLub](l : L, b : mutable.Builder[LLub, M[LLub]], f : Lub => LLub) = {}
        }

    implicit def hnilToTraversableNothing[L <: HNil, M[_]]
      (implicit cbf : Factory[Nothing, M[Nothing]]) : Aux[L, M, Nothing] =
        hnilToTraversable[L, M, Nothing]

    implicit def hsingleToTraversable[T, M[_], Lub0]
      (implicit ev : T <:< Lub0, cbf : Factory[Lub0, M[Lub0]]) : Aux[T :: HNil, M, Lub0] =
        new ToTraversable[T :: HNil, M] {
          type Lub = Lub0
          def builder() = cbf.newBuilder
          def append[LLub](l : T :: HNil, b : mutable.Builder[LLub, M[LLub]], f : Lub0 => LLub) = {
            b += f(l.head)
          }
        }

    implicit def hlistToTraversable[H1, H2, T <: HList, LubT, Lub0, M[_]]
      (implicit
       tttvs  : Aux[H2 :: T, M, LubT],
       u      : Lub[H1, LubT, Lub0],
       cbf    : Factory[Lub0, M[Lub0]]) : Aux[H1 :: H2 :: T, M, Lub0] =
        new ToTraversable[H1 :: H2 :: T, M] {
          type Lub = Lub0
          def builder() = cbf.newBuilder
          def append[LLub](l : H1 :: H2 :: T, b : mutable.Builder[LLub, M[LLub]], f : Lub0 => LLub): Unit = {
            b += f(u.left(l.head)); tttvs.append[LLub](l.tail, b, f compose u.right)
          }
        }
  }

  /**
    * Type class supporting conversion of this `HList` to a `M` with elements typed as
    * a minimal `Coproduct` Cop such as each type of the elements of L appears once in Cop.
    *
    * @author Valentin Kasas
    */
  trait ToCoproductTraversable[L <: HList, M[_]] extends DepFn1[L] with Serializable {
    type Cop <: Coproduct
    type Out = M[Cop]
  }

  trait LowPriorityToCoproductTraversable {
    type Aux[L <: HList, M[_], C <: Coproduct] = ToCoproductTraversable[L, M] {type Cop = C}

    /**
      * Auxiliary type class witnessing that type `T` is not a member of `Coproduct` `C`
      */
    trait NotIn[C <: Coproduct, T]

    object NotIn {
      implicit def notInCNil[T]: NotIn[CNil, T] = new NotIn[CNil, T] {}

      implicit def notInCCons[CH, CT <: Coproduct, T]
        (implicit
         diff: CH =:!= T,
         notInTail: NotIn[CT, T]): NotIn[CH :+: CT, T] = new NotIn[CH :+: CT, T]{}
    }

    implicit def hconsToCoproductTraversable1[LH, LT <: HList, M[_], CT <: Coproduct]
      (implicit
       coproductOfT  : Aux[LT, M, CT],
       cbf           : Factory[CT, M[CT]],
       asTraversable : M[CT] <:< Iterable[CT],
       injectOut     : coproduct.Inject[CT, LH]): Aux[LH :: LT, M, CT] =
        new ToCoproductTraversable[LH :: LT, M] {
          type Cop = CT
          def apply(l : LH :: LT): Out = {
            val builder = cbf.newBuilder
            builder += injectOut(l.head)
            asTraversable(coproductOfT(l.tail)).foreach( builder += _)
            builder.result()
          }
        }
  }

  object ToCoproductTraversable extends LowPriorityToCoproductTraversable{

    implicit def hnilToCoproductTraversable[L <: HNil, M[_]]
    (implicit cbf: Factory[CNil, M[CNil]]): Aux[L, M, CNil] =
      new ToCoproductTraversable[L, M] {
        type Cop = CNil
        def apply(l: L) = cbf.newBuilder.result()
      }


    implicit def hconsToCoproductTraversable0[LH, LT <: HList, M[_], CT <: Coproduct]
      (implicit
       coproductOfT  : Aux[LT, M, CT],
       cbf           : Factory[LH :+: CT, M[LH :+: CT]],
       asTraversable : M[CT] <:< Iterable[CT],
       notIn         : NotIn[CT, LH],
       injectOut     : coproduct.Inject[LH :+: CT, LH],
       basisTail     : coproduct.Basis[LH :+: CT,CT]): Aux[LH :: LT, M, LH :+: CT] =
        new ToCoproductTraversable[LH :: LT, M] {
          type Cop = LH :+: CT
          def apply(l : LH :: LT): Out = {
            val builder = cbf.newBuilder
            builder += injectOut(l.head)
            val tail = coproductOfT(l.tail)
            asTraversable(tail).foreach(elem => builder += elem.embed[Cop])
            builder.result()
          }
        }
  }

  /**
   * Type aliases and constructors provided for backward compatibility
   **/
  type ToArray[L <: HList, Lub] = ToTraversable.Aux[L, Array, Lub]
  def ToArray[L <: HList, Lub](implicit toArray: ToArray[L, Lub]) = toArray

  type ToList[L <: HList, Lub] = ToTraversable.Aux[L, List, Lub]
  def ToList[L <: HList, Lub](implicit toList: ToList[L, Lub]) = toList

  /**
   * Type class supporting conversion of this `HList` to a `Sized[M[Lub], N]` with elements typed
   * as the least upper bound Lub of the types of the elements of this `HList`.
   *
   * About serializability, see the comment in `ToTraversable`.
   *
   * @author Alexandre Archambault
   */
  trait ToSized[L <: HList, M[_]] extends DepFn1[L] with Serializable {
    type Lub
    type N <: Nat
    type Out = Sized[M[Lub], N]
    def apply(l: L): Out
  }

  object ToSized extends ToSizedVersionSpecific {
    def apply[L <: HList, M[_]](implicit toSized: ToSized[L, M]): Aux[L, M, toSized.Lub, toSized.N] = toSized

    type Aux[L <: HList, M[_], Lub0, N0 <: Nat] = ToSized[L, M] { type Lub = Lub0; type N = N0 }

    implicit def hnilToSized[L <: HNil, M[_], T]
      (implicit cbf : Factory[T, M[T]], ev : AdditiveCollection[M[T]]) : Aux[L, M, T, Nat._0] =
        new ToSized[L, M] {
          type Lub = T
          type N = Nat._0
          /* Calling wrap here as Sized[M]() only returns a Sized[M[Nothing], _0] */
          def apply(l : L) = Sized.wrap(cbf.newBuilder.result())
        }

    implicit def hnilToSizedNothing[L <: HNil, M[_]]
      (implicit cbf : Factory[Nothing, M[Nothing]], ev : AdditiveCollection[M[Nothing]]) : Aux[L, M, Nothing, Nat._0] =
        hnilToSized[L, M, Nothing]

    implicit def hsingleToSized[T, M[_], Lub0]
     (implicit ub : T <:< Lub0, cbf : Factory[Lub0, M[Lub0]], ev : AdditiveCollection[M[Lub0]]) : Aux[T :: HNil, M, Lub0, Nat._1] =
      new ToSized[T :: HNil, M] {
        type Lub = Lub0
        type N = Nat._1
        def apply(l : T :: HNil) = Sized[M](l.head)
      }
  }

  /**
   * Type class supporting conversion of this `HList` to a tuple.
   *
   * @author Miles Sabin
   */
  trait Tupler[L <: HList] extends DepFn1[L] with Serializable

  object Tupler extends TuplerInstances {
    def apply[L <: HList](implicit tupler: Tupler[L]): Aux[L, tupler.Out] = tupler

    private[shapeless] def instance[L <: HList, T](tuple: L => T): Aux[L, T] = new Tupler[L] {
      type Out = T
      def apply(l: L) = tuple(l)
    }

    implicit val hnilTupler: Aux[HNil, Unit] =
      instance(_ => ())
  }

  /**
   * Type class supporting access to the last element of this `HList`. Available only if this `HList` has at least one
   * element.
   *
   * @author Miles Sabin
   */
  @implicitNotFound("Implicit not found: shapeless.Ops.Last[${L}]. ${L} is empty, so there is no last element.")
  trait Last[L <: HList] extends DepFn1[L] with Serializable

  object Last {
    def apply[L <: HList](implicit last: Last[L]): Aux[L, last.Out] = last

    type Aux[L <: HList, Out0] = Last[L] { type Out = Out0 }

    implicit def hsingleLast[H]: Aux[H :: HNil, H] =
      new Last[H :: HNil] {
        type Out = H
        def apply(l : H :: HNil): Out = l.head
      }

    implicit def hlistLast[H, T <: HList, OutT]
      (implicit lt : Last.Aux[T, OutT]): Aux[H :: T, OutT] =
        new Last[H :: T] {
          type Out = OutT
          def apply(l : H :: T): Out = lt(l.tail)
        }
  }

  /**
   * Type class supporting access to all but the last element of this `HList`. Available only if this `HList` has at
   * least one element.
   *
   * @author Miles Sabin
   */
  @implicitNotFound("Implicit not found: shapeless.Ops.Init[${L}]. {L} is empty, so there is no first element.")
  trait Init[L <: HList] extends DepFn1[L] with Serializable { type Out <: HList }

  object Init {
    def apply[L <: HList](implicit init: Init[L]): Aux[L, init.Out] = init

    type Aux[L <: HList, Out0 <: HList] = Init[L] { type Out = Out0 }

    implicit def hsingleInit[H]: Aux[H :: HNil, HNil] =
      new Init[H :: HNil] {
        type Out = HNil
        def apply(l : H :: HNil): Out = HNil
      }

    implicit def hlistInit[H, T <: HList, OutH, OutT <: HList]
      (implicit it : Init.Aux[T, OutT]): Aux[H :: T, H :: OutT] =
        new Init[H :: T] {
          type Out = H :: OutT
          def apply(l : H :: T): Out = l.head :: it(l.tail)
        }
  }

  /**
   * Type class supporting access to the first element of this `HList` of type `U`. Available only if this `HList`
   * contains an element of type `U`.
   *
   * @author Miles Sabin
   */
  @implicitNotFound("Implicit not found: shapeless.Ops.Selector[${L}, ${U}]. You requested an element of type ${U}, but there is none in the HList ${L}.")
  trait Selector[L <: HList, U] extends DepFn1[L] with Serializable { type Out = U }

  object Selector {

    def apply[L <: HList, U](implicit selector: Selector[L, U]): Selector[L, U] = selector

    implicit def select[H, T <: HList]: Selector[H :: T, H] =
      new Selector[H :: T, H] {
        def apply(l : H :: T) = l.head
      }

    implicit def recurse[H, T <: HList, U]
      (implicit st : Selector[T, U]): Selector[H :: T, U] =
        new Selector[H :: T, U] {
          def apply(l : H :: T) = st(l.tail)
        }
  }

  /**
    * Type class supporting multiple HList field selection.
    * Can be used to witness that given HList contains certain set of field types.
    * Simplified version of shapeless.ops.record.SelectAll
    *
    * @author Ievgen Garkusha
    */
  @annotation.implicitNotFound(msg = "Field types set of ${S} is not fully contained in type definition of ${L}")
  trait SelectAll[L <: HList, S <: HList] extends DepFn1[L] with Serializable { type Out = S }

  object SelectAll {

    def apply[L <: HList, S <: HList](implicit select: SelectAll[L, S]): SelectAll[L, S] = select

    implicit def hnilSelectAll[L <: HList]: SelectAll[L, HNil] =
      new SelectAll[L, HNil] {
        def apply(l: L): Out = HNil
      }

    implicit def hconsSelectAll[L <: HList, H, S <: HList]
    (implicit sh: Selector[L, H], st: SelectAll[L, S]): SelectAll[L, H :: S] =
      new SelectAll[L, H :: S] {
        def apply(l: L): Out = sh(l) :: st(l)
      }
  }

  /**
   * Type class supporting access to the elements of this `HList` specified by `Ids`. Available only if this `HList`
   * contains all elements specified in `Ids`.
   *
   * @author Andreas Koestler
   */
  @implicitNotFound("Implicit not found: shapeless.Ops.SelectMany[${L}, ${Ids}]. You requested the elements in ${Ids}, but HList ${L} does not contain all of them.")
  trait SelectMany[L <: HList, Ids <: HList] extends DepFn1[L] { type Out <: HList }

  object SelectMany {
    def apply[L <: HList, Ids <: HList](implicit sel: SelectMany[L, Ids]): Aux[L, Ids, sel.Out] = sel

    type Aux[L <: HList, Ids <: HList, Out0 <: HList] = SelectMany[L, Ids] { type Out = Out0 }

    implicit def SelectManyHNil[L <: HList]: Aux[L, HNil, HNil] =
      new SelectMany[L, HNil] {
        type Out = HNil
        def apply(l : L): Out = HNil
      }

    implicit def SelectManyHList[L <:HList, H <: Nat, T <: HList, SelOut <: HList, AtOut]
    (implicit sel : SelectMany.Aux[L, T, SelOut], at: At.Aux[L, H, AtOut]): Aux[L, H::T, AtOut :: SelOut] =
      new SelectMany[L, H::T] {
        type Out = AtOut :: SelOut
        def apply(l: L): Out = at(l) :: sel(l)
      }
  }

  /**
   * Type class supporting supporting access to the elements in range [a,b] of this `HList`.
   * Available only if this `HList` contains all elements in range
   *
   * @author Ivan Aristov
   */
  @implicitNotFound("Implicit not found: shapeless.Ops.SelectRange[${L}, ${A}, ${B}]. You requested the elements in range [${A},${B}[, but HList ${L} does not contain all of them.")
  trait SelectRange[L <: HList, A <: Nat, B <: Nat] extends DepFn1[L] { type Out <: HList }

  object SelectRange {
    def apply[L <: HList, A <: Nat, B <: Nat](implicit sel: SelectRange[L, A, B]): Aux[L, A, B, sel.Out] = sel

    type Aux[L <: HList, A <: Nat, B <: Nat, Out0 <: HList] = SelectRange[L, A, B] {
      type Out = Out0
    }

    @deprecated("Slower version of take and drop", "2.3.5")
    def SelectRangeAux[L <: HList, A <: Nat, B <: Nat, Ids <: HList, SelOut <: HList](
      implicit range: nat.Range.Aux[A, B, Ids], sel: SelectMany.Aux[L, Ids, SelOut]
    ): Aux[L, A, B, SelOut] = new SelectRange[L, A, B] {
      type Out = SelOut
      def apply(l: L): Out = sel(l)
    }

    implicit def take[L <: HList, T <: Nat](
      implicit take: Take[L, T]
    ): Aux[L, _0, T, take.Out] = new SelectRange[L, _0, T] {
      type Out = take.Out
      def apply(t: L): take.Out = take(t)
    }

    implicit def drop[H, L <: HList, A <: Nat, B <: Nat](
      implicit select: SelectRange[L, A, B]
    ): Aux[H :: L, Succ[A], Succ[B], select.Out] = new SelectRange[H :: L, Succ[A], Succ[B]] {
      type Out = select.Out
      def apply(t: H :: L): select.Out = select(t.tail)
    }
  }



  /**
   * Type class supporting partitioning this `HList` into those elements of type `U` and the
   * remainder
   *
   * @author Stacy Curl
   */
  trait Partition[L <: HList, U] extends DepFn1[L] with Serializable {
    type Prefix <: HList
    type Suffix <: HList
    type Out = (Prefix, Suffix)

    def apply(l: L): Out = toTuple2(product(l))
    def product(l: L): Prefix :: Suffix :: HNil = filter(l) :: filterNot(l) :: HNil
    def filter(l: L): Prefix
    def filterNot(l: L): Suffix
  }

  object Partition {
    def apply[L <: HList, U]
      (implicit partition: Partition[L, U]): Aux[L, U, partition.Prefix, partition.Suffix] = partition

    type Aux[L <: HList, U, Prefix0 <: HList, Suffix0 <: HList] = Partition[L, U] {
      type Prefix = Prefix0
      type Suffix = Suffix0
    }

    implicit def hlistPartitionNil[U]: Aux[HNil, U, HNil, HNil] = new Partition[HNil, U] {
      type Prefix = HNil
      type Suffix = HNil

      def filter(l: HNil): HNil = HNil
      def filterNot(l: HNil): HNil = HNil
    }

    implicit def hlistPartition1[H, L <: HList, LPrefix <: HList, LSuffix <: HList](
      implicit p: Aux[L, H, LPrefix, LSuffix]
    ): Aux[H :: L, H, H :: LPrefix, LSuffix] = new Partition[H :: L, H] {
      type Prefix = H :: LPrefix
      type Suffix = LSuffix

      def filter(l: H :: L): Prefix    = l.head :: p.filter(l.tail)
      def filterNot(l: H :: L): Suffix = p.filterNot(l.tail)
    }

    @deprecated("Slower version of hlistPartition3", "2.3.5")
    def hlistPartition2[H, L <: HList, U, LPrefix <: HList, LSuffix <: HList](
      implicit p: Aux[L, U, LPrefix, LSuffix], e: U =:!= H
    ): Aux[H :: L, U, LPrefix, H :: LSuffix] = hlistPartition3

    implicit def hlistPartition3[H, L <: HList, U, LPrefix <: HList, LSuffix <: HList](
      implicit e: U =:!= H, p: Aux[L, U, LPrefix, LSuffix]
    ): Aux[H :: L, U, LPrefix, H :: LSuffix] = new Partition[H :: L, U] {
      type Prefix = LPrefix
      type Suffix = H :: LSuffix

      def filter(l: H :: L): Prefix    = p.filter(l.tail)
      def filterNot(l: H :: L): Suffix = l.head :: p.filterNot(l.tail)
    }
  }


 /**
  * Typeclass supporting grouping this `HList` into tuples of `N` items each, at `Step`
  * apart. If `Step` equals `N` then the groups do not overlap.
  *
  * @author Andreas Koestler
  */
 trait Grouper[L <: HList, N <: Nat, Step <: Nat] extends DepFn1[L] with Serializable {
    type Out <: HList
  }

  trait LowPriorityGrouper {

    implicit def hlistGrouper[L <: HList, N <: Nat, OutT <: HList, OutD <: HList, T, Step <: Nat, LL <: Nat, M <: Nat, OutG <: HList]
    (implicit
     len: Length.Aux[L, LL],
     min: ops.nat.Min.Aux[LL, Step, M],
     take: ops.hlist.Take.Aux[L, N, OutT],
     drop: ops.hlist.Drop.Aux[L, M, OutD],
     tup: ops.hlist.Tupler.Aux[OutT, T],
     grouper: Grouper.Aux[OutD, N, Step, OutG]
    ): Grouper.Aux[L, N, Step, T :: OutG] = new Grouper[L, N, Step] {
      type Out = T :: OutG

      def apply(l: L) = tup(take(l)) :: grouper(drop(l))
    }
  }

  object Grouper extends LowPriorityGrouper {
    def apply[L <: HList, N <: Nat, Step <: Nat](implicit
                                                 grouper: Grouper[L, N, Step]
                                                  ): Aux[L, N, Step, grouper.Out] = grouper

    type Aux[L <: HList, N <: Nat, Step <: Nat, Out0 <: HList] = Grouper[L, N, Step] {
      type Out = Out0
    }

    implicit def hnilGrouper[N <: Nat, Step <: Nat]: Aux[HNil, N, Step, HNil] = new Grouper[HNil, N, Step] {
      type Out = HNil

      def apply(l: HNil): Out = HNil
    }

    implicit def hlistGrouper1[L <: HList, A <: Nat, N <: Nat, Step <: Nat](implicit
                                                                            len: ops.hlist.Length.Aux[L, A],
                                                                            lt: ops.nat.LT[A, N]
                                                                             ): Aux[L, N, Step, HNil] = new Grouper[L, N, Step] {
      type Out = HNil

      def apply(l: L): Out = HNil
    }
  }

  /**
   * Typeclass supporting grouping this `HList` into tuples of `N` items each, at `Step`
   * apart. If `Step` equals `N` then the groups do not overlap.
   *
   * Use elements in `pad` as necessary to complete last group up to `n` items.
   *
   * @author Andreas Koestler
   */
  trait LowPriorityPaddedGrouper {

    implicit def incompletePaddedGrouper[
    L <: HList,
    N <: Nat,
    Step <: Nat,
    Pad <: HList,
    PL <: HList,
    LPL <: Nat,
    S <: Nat,
    MI <: Nat,
    T <: HList,
    M <: Nat,
    LL <: Nat,
    Out0 <: HList]
    (implicit
     len1: Length.Aux[L, LL],
     prep: Prepend.Aux[L, Pad, PL],
     len2: Length.Aux[PL, LPL],
     mod: ops.nat.Mod.Aux[LL, Step, M],
     sum: ops.nat.Sum.Aux[LL, M, S],
     min: ops.nat.Min.Aux[LPL, S, MI],
     take: Take.Aux[PL, MI, T],
     grouper: Grouper.Aux[T, N, Step, Out0]
      ): PaddedGrouper.Aux[L, N, Step, Pad, Out0] = new PaddedGrouper[L, N, Step, Pad] {

      type Out = Out0

      def apply(l: L, pad: Pad): Out = grouper(take(prep(l, pad)))
    }

  }

  trait PaddedGrouper[L <: HList, N <: Nat, Step <: Nat, Pad <: HList] extends DepFn2[L, Pad] with Serializable {
    type Out <: HList
  }

  object PaddedGrouper extends LowPriorityPaddedGrouper {
    def apply[L <: HList, N <: Nat, Step <: Nat, Pad <: HList](implicit
                                                               grouper: PaddedGrouper[L, N, Step, Pad]
                                                                ): Aux[L, N, Step, Pad, grouper.Out] = grouper

    type Aux[L <: HList, N <: Nat, Step <: Nat, Pad <: HList, Out0] = PaddedGrouper[L, N, Step, Pad] {
      type Out = Out0
    }

    implicit def defaultPaddedGrouper[L <: HList, N <: Nat, Step <: Nat, Pad <: HList, A <: Nat, B <: Nat, Out0 <: HList]
    (implicit
     len: Length.Aux[L, A],
     mod: ops.nat.Mod.Aux[A, Step, B],
     eq: B =:= _0,
     grouper: Grouper.Aux[L, N, Step, Out0]
      ): Aux[L, N, Step, Pad, Out0] = new PaddedGrouper[L, N, Step, Pad] {

      type Out = grouper.Out

      def apply(l: L, pad: Pad): Out = grouper(l)
    }

  }


  /**
   * Type class supporting access to the all elements of this `HList` of type `U`.
   *
   * @author Alois Cochard
   */
  trait Filter[L <: HList, U] extends DepFn1[L] with Serializable { type Out <: HList }

  object Filter {
    def apply[L <: HList, U](implicit filter: Filter[L, U]): Aux[L, U, filter.Out] = filter

    type Aux[L <: HList, U, Out0 <: HList] = Filter[L, U] { type Out = Out0 }

    implicit def hlistFilter[L <: HList, U, LPrefix <: HList, LSuffix <: HList](
      implicit partition: Partition.Aux[L, U, LPrefix, LSuffix]
    ): Aux[L, U, LPrefix] = new Filter[L, U] {
      type Out = LPrefix

      def apply(l: L): Out = partition.filter(l)
    }
  }

  /**
   * Type class supporting access to the all elements of this `HList` of type different than `U`.
   *
   * @author Alois Cochard
   */
  trait FilterNot[L <: HList, U] extends DepFn1[L] with Serializable { type Out <: HList }

  object FilterNot {
    def apply[L <: HList, U](implicit filter: FilterNot[L, U]): Aux[L, U, filter.Out] = filter

    type Aux[L <: HList, U, Out0 <: HList] = FilterNot[L, U] { type Out = Out0 }

    implicit def hlistFilterNot[L <: HList, U, LPrefix <: HList, LSuffix <: HList](
      implicit partition: Partition.Aux[L, U, LPrefix, LSuffix]
    ): Aux[L, U, LSuffix] = new FilterNot[L, U] {
      type Out = LSuffix

      def apply(l: L): Out = partition.filterNot(l)
    }
  }

  /**
   * Type class supporting removal of an element from this `HList`. Available only if this `HList` contains an
   * element of type `E`.
   *
   * @author Stacy Curl
   */
  @implicitNotFound("Implicit not found: shapeless.Ops.Remove[${L}, ${E}]. You requested to remove an element of type ${E}, but there is no unique candidate in the HList ${L}.")
  trait Remove[L <: HList, E] extends DepFn1[L] with Serializable {
    def reinsert(out: Out): L
  }

  trait LowPriorityRemove {
    type Aux[L <: HList, E, Out0] = Remove[L, E] { type Out = Out0 }

    implicit def recurse[H, T <: HList, E, OutT <: HList](implicit r : Aux[T, E, (E, OutT)]): Aux[H :: T, E, (E, H :: OutT)] =
      new Remove[H :: T, E] {
        type Out = (E, H :: OutT)
        def apply(l : H :: T): Out = {
          val (e, tail) = r(l.tail)
          (e, l.head :: tail)
        }

        def reinsert(out: (E, H :: OutT)): H :: T = out._2.head :: r.reinsert((out._1, out._2.tail))
      }
  }

  object Remove extends LowPriorityRemove {
    def apply[L <: HList, E](implicit remove: Remove[L, E]): Aux[L, E, remove.Out] = remove

    implicit def remove[H, T <: HList]: Aux[H :: T, H, (H, T)] =
      new Remove[H :: T, H] {
        type Out = (H, T)
        def apply(l : H :: T): Out = (l.head, l.tail)

        def reinsert(out: (H, T)): H :: T = out._1 :: out._2
      }
  }

  /**
   * Type class supporting removal of a sublist from this `HList`. Available only if this `HList` contains a
   * sublist of type `SL`.
   *
   * The elements of `SL` do not have to be contiguous in this `HList`.
   *
   * @author Stacy Curl
   */
  @implicitNotFound("Implicit not found: shapeless.Ops.RemoveAll[${L}, ${SL}]. You requested to remove elements of the types ${SL}, but not all were found in HList ${L}.")
  trait RemoveAll[L <: HList, SL <: HList] extends DepFn1[L] with Serializable {
    def reinsert(out: Out): L
  }

  object RemoveAll {
    def apply[L <: HList, SL <: HList](implicit remove: RemoveAll[L, SL]): Aux[L, SL, remove.Out] = remove

    type Aux[L <: HList, SL <: HList, Out0] = RemoveAll[L, SL] { type Out = Out0 }

    implicit def hlistRemoveAllNil[L <: HList]: Aux[L, HNil, (HNil, L)] =
      new RemoveAll[L, HNil] {
        type Out = (HNil, L)
        def apply(l : L): Out = (HNil, l)

        def reinsert(out: (HNil, L)): L = out._2
      }

    implicit def hlistRemoveAll[L <: HList, E, RemE <: HList, Rem <: HList, SLT <: HList]
      (implicit rt : Remove.Aux[L, E, (E, RemE)], st : Aux[RemE, SLT, (SLT, Rem)]): Aux[L, E :: SLT, (E :: SLT, Rem)] =
        new RemoveAll[L, E :: SLT] {
          type Out = (E :: SLT, Rem)
          def apply(l : L): Out = {
            val (e, rem) = rt(l)
            val (sl, left) = st(rem)
            (e :: sl, left)
          }

          def reinsert(out: (E :: SLT, Rem)): L =
            rt.reinsert((out._1.head, st.reinsert((out._1.tail, out._2))))
        }
  }

  /**
   * Type class supporting `HList` union. In case of duplicate types, this operation is a order-preserving multi-set union.
   * If type `T` appears n times in this `HList` and m > n times in `M`, the resulting `HList` contains the first n elements
   * of type `T` in this `HList`, followed by the last m - n element of type `T` in `M`.
   *
   * @author Olivier Blanvillain
   * @author Arya Irani
   */
  trait Union[L <: HList, M <: HList] extends DepFn2[L, M] with Serializable { type Out <: HList }

  trait LowPriorityUnion {
    type Aux[L <: HList, M <: HList, Out0 <: HList] = Union[L, M] { type Out = Out0 }

    // buggy version; let (H :: T) ∪ M = H :: (T ∪ M)
    @deprecated("Incorrectly witnesses that {x} ∪ {x} = {x, x}", "2.3.1")
    def hlistUnion1[H, T <: HList, M <: HList, U <: HList](
      implicit u: Union.Aux[T, M, U]
    ): Aux[H :: T, M, H :: U] = new Union[H :: T, M] {
      type Out = H :: U
      def apply(l: H :: T, m: M): Out = l.head :: u(l.tail, m)
    }
  }

  object Union extends LowPriorityUnion {
    def apply[L <: HList, M <: HList](implicit union: Union[L, M]): Aux[L, M, union.Out] = union

    // let ∅ ∪ M = M
    implicit def hlistUnion[M <: HList]: Aux[HNil, M, M] =
      new Union[HNil, M] {
        type Out = M
        def apply(l: HNil, m: M): Out = m
      }

    // let (H :: T) ∪ M = H :: (T ∪ M) when H ∉ M
    implicit def hlistUnion0[H, T <: HList, M <: HList, U <: HList](
      implicit f: NotContainsConstraint[M, H], u: Union.Aux[T, M, U]
    ): Aux[H :: T, M, H :: U] = new Union[H :: T, M] {
      type Out = H :: U
      def apply(l: H :: T, m: M): Out = l.head :: u(l.tail, m)
    }

    // let (H :: T) ∪ M = H :: (T ∪ M) when H ∉ M
    @deprecated("Slower version of hlistUnion0", "2.3.5")
    def hlistUnion1[H, T <: HList, M <: HList, U <: HList](
      implicit u: Union.Aux[T, M, U], f: FilterNot.Aux[M, H, M]
    ): Aux[H :: T, M, H :: U] = new Union[H :: T, M] {
      type Out = H :: U
      def apply(l: H :: T, m: M): Out = l.head :: u(l.tail, m)
    }

    // let (H :: T) ∪ M  =  H :: (T ∪ (M - H)) when H ∈ M
    implicit def hlistUnion2[H, T <: HList, M <: HList, MR <: HList, U <: HList]
      (implicit
        r: Remove.Aux[M, H, (H, MR)],
        u: Union.Aux[T, MR, U]
      ): Aux[H :: T, M, H :: U] =
        new Union[H :: T, M] {
          type Out = H :: U
          def apply(l: H :: T, m: M): Out = l.head :: u(l.tail, r(m)._2)
        }
  }

  /**
   * Type class supporting `HList` intersection. In case of duplicate types, this operation is a multiset intersection.
   * If type `T` appears n times in this `HList` and m < n times in `M`, the resulting `HList` contains the first m
   * elements of type `T` in this `HList`.
   *
   * Also available if `M` contains types absent in this `HList`.
   *
   * @author Olivier Blanvillain
   * @author Arya Irani
   */
  trait Intersection[L <: HList, M <: HList] extends DepFn1[L] with Serializable { type Out <: HList }

  trait LowPriorityIntersection {
    type Aux[L <: HList, M <: HList, Out0 <: HList] = Intersection[L, M] { type Out = Out0 }

    // buggy version; let (H :: T) ∩ M = T ∩ M
    @deprecated("Incorrectly witnesses that {x} ∩ M = ∅", "2.3.1")
    def hlistIntersection1[H, T <: HList, M <: HList, I <: HList](
      implicit i: Intersection.Aux[T, M, I]
    ): Aux[H :: T, M, I] = new Intersection[H :: T, M] {
      type Out = I
      def apply(l: H :: T): Out = i(l.tail)
    }
  }

  object Intersection extends LowPriorityIntersection {
    def apply[L <: HList, M <: HList](implicit intersection: Intersection[L, M]): Aux[L, M, intersection.Out] = intersection

    // let ∅ ∩ M = ∅
    implicit def hnilIntersection[M <: HList]: Aux[HNil, M, HNil] =
      new Intersection[HNil, M] {
        type Out = HNil
        def apply(l: HNil): Out = HNil
      }

    // let (H :: T) ∩ M = T ∩ M when H ∉ M
    implicit def hlistIntersection0[H, T <: HList, M <: HList, I <: HList](
      implicit f: NotContainsConstraint[M, H], i: Intersection.Aux[T, M, I]
    ): Aux[H :: T, M, I] = new Intersection[H :: T, M] {
      type Out = I
      def apply(l: H :: T): Out = i(l.tail)
    }

    // let (H :: T) ∩ M = T ∩ M when H ∉ M
    @deprecated("Slower version of hlistIntersection0", "2.3.5")
    def hlistIntersection1[H, T <: HList, M <: HList, I <: HList](
      implicit i: Intersection.Aux[T, M, I], f: FilterNot.Aux[M, H, M]
    ): Aux[H :: T, M, I] = new Intersection[H :: T, M] {
      type Out = I
      def apply(l: H :: T): Out = i(l.tail)
    }

    // let (H :: T) ∩ M  =  H :: (T ∩ (M - H)) when H ∈ M
    implicit def hlistIntersection2[H, T <: HList, M <: HList, MR <: HList, I <: HList]
      (implicit
        r: Remove.Aux[M, H, (H, MR)],
        i: Intersection.Aux[T, MR, I]
      ): Aux[H :: T, M, H :: I] =
        new Intersection[H :: T, M] {
          type Out = H :: I
          def apply(l: H :: T): Out = l.head :: i(l.tail)
        }
  }

  /**
   * Type class supporting `HList` subtraction. In case of duplicate types, this operation is a multiset difference.
   * If type `T` appears n times in this `HList` and m < n times in `M`, the resulting `HList` contains the last n - m
   * elements of type `T` in this `HList`.
   *
   * Also available if `M` contains types absent in this `HList`.
   *
   * @author Olivier Blanvillain
   */
  trait Diff[L <: HList, M <: HList] extends DepFn1[L] with Serializable { type Out <: HList }

  trait LowPriorityDiff {
    type Aux[L <: HList, M <: HList, Out0] = Diff[L, M] { type Out = Out0 }

    implicit def hconsDiff1[L <: HList, H, T <: HList, D <: HList]
      (implicit d: Diff.Aux[L, T, D]): Aux[L, H :: T, D] =
        new Diff[L, H :: T] {
          type Out = D
          def apply(l: L): Out = d(l)
        }
  }

  object Diff extends LowPriorityDiff {
    def apply[L <: HList, M <: HList](implicit diff: Diff[L, M]): Aux[L, M, diff.Out] = diff

    implicit def hnilDiff[L <: HList]: Aux[L, HNil, L] =
      new Diff[L, HNil] {
        type Out = L
        def apply(l: L): Out = l
      }

    implicit def hconsDiff2[L <: HList, LT <: HList, H, T <: HList, D <: HList]
      (implicit
        r: Remove.Aux[L, H, (H, LT)],
        d: Diff.Aux[LT, T, D]
      ): Aux[L, H :: T, D] =
        new Diff[L, H :: T] {
          type Out = D
          def apply(l: L): Out = d(r(l)._2)
        }
  }

  /**
   * Type class supporting replacement of the first element of type U from this `HList` with an element of type V.
   * Available only if this `HList` contains an element of type `U`.
   *
   * @author Miles Sabin
   */
  @implicitNotFound("Implicit not found: shapeless.Ops.Replacer[${L}, ${U}, ${V}]. You requested to replace an element of the type ${U}, but there is none in HList ${L}.")
  trait Replacer[L <: HList, U, V] extends DepFn2[L, V] with Serializable

  object Replacer {
    def apply[L <: HList, U, V](implicit replacer: Replacer[L, U, V]): Aux[L, U, V, replacer.Out] = replacer

    type Aux[L <: HList, U, V, Out0] = Replacer[L, U, V] { type Out = Out0 }

    implicit def hlistReplacer1[T <: HList, U, V]: Aux[U :: T, U, V, (U, V :: T)] =
      new Replacer[U :: T, U, V] {
        type Out = (U, V :: T)
        def apply(l : U :: T, v : V): Out = (l.head, v :: l.tail)
      }

    implicit def hlistReplacer2[H, T <: HList, U, V, OutT <: HList]
      (implicit ut : Aux[T, U, V, (U, OutT)]): Aux[H :: T, U, V, (U, H :: OutT)] =
        new Replacer[H :: T, U, V] {
          type Out = (U, H :: OutT)
          def apply(l : H :: T, v : V): Out = {
            val (u, outT) = ut(l.tail, v)
            (u, l.head :: outT)
          }
        }
  }

  /**
   * Type class supporting replacement of the first element of type U from this `HList` with the result of
   * its transformation via a given function into a new element of type V.
   * Available only if this `HList` contains an element of type `U`.
   *
   * @author Jules Gosnell
   */
  @implicitNotFound("Implicit not found: shapeless.Ops.Modifier[${L}, ${U}, ${V}]. You requested to modify an element of the type ${U}, but there is none in HList ${L}.")
  trait Modifier[L <: HList, U, V] extends DepFn2[L, U => V] with Serializable

  object Modifier {
    def apply[L <: HList, U, V](implicit modifier: Modifier[L, U, V]): Aux[L, U, V, modifier.Out] = modifier

    type Aux[L <: HList, U, V, Out0] = Modifier[L, U, V] { type Out = Out0 }

    implicit def hlistModify1[T <: HList, U, V]: Aux[U :: T, U, V, (U, V :: T)] =
      new Modifier[U :: T, U, V] {
        type Out = (U, V :: T)
        def apply(l : U :: T, f : U => V): Out = {val u = l.head; (u, f(u) :: l.tail)}
      }

    implicit def hlistModify2[H, T <: HList, U, V, OutT <: HList]
      (implicit ut : Aux[T, U, V, (U, OutT)]): Aux[H :: T, U, V, (U, H :: OutT)] =
        new Modifier[H :: T, U, V] {
          type Out = (U, H :: OutT)

          def apply(l : H :: T, f : U => V): Out = {
            val (u, outT) = ut(l.tail, f)
            (u, l.head :: outT)
          }
        }
  }

  /**
   * Type class supporting replacement of the `N`th element of this `HList` with the result of
   * calling `F` on it.
   * Available only if this `HList` contains at least `N` elements.
   *
   * @author Andreas Koestler
   */
  trait ModifierAt[L <: HList, N <: Nat, U, V] extends DepFn2[L, U => V]

  object ModifierAt {
    def apply[L <: HList, N <: Nat, U, V](implicit modifier: ModifierAt[L, N, U, V]): Aux[L, N, U, V, modifier.Out] = modifier

    type Aux[L <: HList, N <: Nat, U, V, Out0] = ModifierAt[L, N, U, V] {type Out = Out0}

    implicit def hlistModifierAt[L <: HList, OutL <: HList, N <: Nat, F, U, V]
    (implicit
     at: At.Aux[L, N, U],
     replacer: ReplaceAt.Aux[L, N, V, (U, OutL)]
      ): Aux[L, N, U, V, (U, OutL)] =
      new ModifierAt[L, N, U, V] {
        type Out = (U, OutL)

        def apply(l: L, f: U => V): Out = {
          val (u, res) = replacer(l, f(at(l)))
          (u, res)
        }
      }
  }

  /**
   * Type class supporting replacement of the Nth element of this `HList` with an element of type V. Available only if
   * this `HList` contains at least N elements.
   *
   * @author Miles Sabin
   */
  @implicitNotFound("Implicit not found: shapeless.Ops.ReplaceAt[${L}, ${N}, ${V}]. You requested to modify an element at the position ${N}, but the HList ${L} is too short.")
  trait ReplaceAt[L <: HList, N <: Nat, V] extends DepFn2[L, V] with Serializable

  object ReplaceAt {
    def apply[L <: HList, N <: Nat, V](implicit replacer: ReplaceAt[L, N, V]): Aux[L, N, V, replacer.Out] = replacer

    type Aux[L <: HList, N <: Nat, V, Out0] = ReplaceAt[L, N, V] { type Out = Out0 }

    implicit def hlistReplaceAt1[H, T <: HList, V]: Aux[H :: T, _0, V, (H, V :: T)] =
      new ReplaceAt[H :: T, _0, V] {
        type Out = (H, V :: T)
        def apply(l : H :: T, v : V): Out = (l.head, v :: l.tail)
      }

    implicit def hlistReplaceAt2[H, T <: HList, N <: Nat, U, V, Out0 <: HList]
      (implicit ut : Aux[T, N, V, (U, Out0)]): Aux[H :: T, Succ[N], V, (U, H :: Out0)] =
        new ReplaceAt[H :: T, Succ[N], V] {
          type Out = (U, H :: Out0)
          def apply(l : H :: T, v : V): Out = {
            val (u, outT) = ut(l.tail, v)
            (u, l.head :: outT)
          }
        }
  }

  /**
   * Type class supporting access to the ''nth'' element of this `HList`. Available only if this `HList` has at least
   * ''n'' elements.
   *
   * @author Miles Sabin
   */
  @implicitNotFound("Implicit not found: shapeless.Ops.At[${L}, ${N}]. You requested to access an element at the position ${N}, but the HList ${L} is too short.")
  trait At[L <: HList, N <: Nat] extends DepFn1[L] with Serializable

  object At {
    def apply[L <: HList, N <: Nat](implicit at: At[L, N]): Aux[L, N, at.Out] = at

    type Aux[L <: HList, N <: Nat, Out0] = At[L, N] { type Out = Out0 }

    implicit def hlistAtZero[H, T <: HList]: Aux[H :: T, _0, H] =
      new At[H :: T, _0] {
        type Out = H
        def apply(l : H :: T): Out = l.head
      }

    implicit def hlistAtN[H, T <: HList, N <: Nat, AtOut]
      (implicit att : At.Aux[T, N, AtOut]): Aux[H :: T, Succ[N], AtOut] =
        new At[H :: T, Succ[N]] {
          type Out = AtOut
          def apply(l : H :: T) : Out = att(l.tail)
        }
  }

  /**
   * Type class supporting removal of the first ''n'' elements of this `HList`. Available only if this `HList` has at
   * least ''n'' elements.
   *
   * @author Miles Sabin
   */
  @implicitNotFound("Implicit not found: shapeless.Ops.Drop[${L}, ${N}]. You requested to drop an element at the position ${N}, but the HList ${L} is too short.")
  trait Drop[L <: HList, N <: Nat] extends DepFn1[L] with Serializable { type Out <: HList }

  object Drop {
    def apply[L <: HList, N <: Nat](implicit drop: Drop[L, N]): Aux[L, N, drop.Out] = drop

    type Aux[L <: HList, N <: Nat, Out0 <: HList] = Drop[L, N] { type Out = Out0 }

    implicit def hlistDrop1[L <: HList]: Aux[L, _0, L] =
      new Drop[L, _0] {
        type Out = L
        def apply(l : L): Out = l
      }

    implicit def hlistDrop2[H, T <: HList, N <: Nat, Out0 <: HList]
      (implicit dt : Drop.Aux[T, N, Out0]): Aux[H :: T, Succ[N], Out0] =
        new Drop[H :: T, Succ[N]] {
          type Out = Out0
          def apply(l : H :: T): Out = dt(l.tail)
        }
  }

  /**
   * Type class supporting retrieval of the first ''n'' elements of this `HList`. Available only if this `HList` has at
   * least ''n'' elements.
   *
   * @author Miles Sabin
   */
  @implicitNotFound("Implicit not found: shapeless.Ops.Take[${L}, ${N}]. You requested to take ${N} elements, but the HList ${L} is too short.")
  trait Take[L <: HList, N <: Nat] extends DepFn1[L] with Serializable { type Out <: HList }

  object Take {
    def apply[L <: HList, N <: Nat](implicit take: Take[L, N]): Aux[L, N, take.Out] = take

    type Aux[L <: HList, N <: Nat, Out0 <: HList] = Take[L, N] { type Out = Out0 }

    implicit def hlistTake1[L <: HList]: Aux[L, _0, HNil] =
      new Take[L, _0] {
        type Out = HNil
        def apply(l : L): Out = HNil
      }

    implicit def hlistTake2[H, T <: HList, N <: Nat, Out0 <: HList]
      (implicit tt : Take.Aux[T, N, Out0]): Aux[H :: T, Succ[N], H :: Out0] =
        new Take[H :: T, Succ[N]] {
          type Out = H :: Out0
          def apply(l : H :: T): Out = l.head :: tt(l.tail)
        }
  }


  /**
   * Type class supporting splitting this `HList` at the ''nth'' element returning the prefix and suffix as a pair.
   * Available only if this `HList` has at least ''n'' elements.
   *
   * @author Miles Sabin
   */
  @implicitNotFound("Implicit not found: shapeless.Ops.Split[${L}, ${N}]. You requested to split at position ${N}, but the HList ${L} is too short.")
  trait Split[L <: HList, N <: Nat] extends DepFn1[L] with Serializable {
    type Prefix <: HList
    type Suffix <: HList
    type Out = (Prefix, Suffix)

    def apply(l: L): Out = toTuple2(product(l))
    def product(l: L): Prefix :: Suffix :: HNil
  }

  object Split {
    def apply[L <: HList, N <: Nat](implicit split: Split[L, N]): Aux[L, N, split.Prefix, split.Suffix] = split

    type Aux[L <: HList, N <: Nat, Prefix0 <: HList, Suffix0 <: HList] = Split[L, N] {
      type Prefix = Prefix0
      type Suffix = Suffix0
    }

    implicit def split[L <: HList, N <: Nat, P <: HList, S <: HList]
      (implicit split : Split0[HNil, L, N, P, S]): Aux[L, N, P, S] =
        new Split[L, N] {
          type Prefix = P
          type Suffix = S
          def product(l : L): Prefix :: Suffix :: HNil = split(HNil, l)
        }

    trait Split0[AccP <: HList, AccS <: HList, N <: Nat, P <: HList, S <: HList] extends Serializable {
      def apply(accP : AccP, accS : AccS) : P :: S :: HNil
    }

    object Split0 {
      implicit def hlistSplit1[P <: HList, S <: HList]: Split0[P, S, _0, P, S] =
        new Split0[P, S, _0, P, S] {
          def apply(accP : P, accS : S) : P :: S :: HNil = accP :: accS :: HNil
        }

      implicit def hlistSplit2[AccP <: HList, AccSH, AccST <: HList, N <: Nat, P <: HList, S <: HList]
        (implicit st : Split0[AccP, AccST, N, P, S]): Split0[AccP, AccSH :: AccST, Succ[N], AccSH :: P, S] =
          new Split0[AccP, AccSH :: AccST, Succ[N], AccSH :: P, S] {
            def apply(accP : AccP, accS : AccSH :: AccST) : (AccSH :: P) :: S :: HNil =
              st(accP, accS.tail) match {
                case prefix :: suffix :: HNil => (accS.head :: prefix) :: suffix :: HNil
              }
          }
    }
  }

  /**
   * Type class supporting splitting this `HList` at the ''nth'' element returning the reverse prefix and suffix as a
   * pair. Available only if this `HList` has at least ''n'' elements.
   *
   * @author Miles Sabin
   */
  @implicitNotFound("Implicit not found: shapeless.Ops.ReverseSplit[${L}, ${N}]. You requested to split at position ${N}, but the HList ${L} is too short.")
  trait ReverseSplit[L <: HList, N <: Nat] extends DepFn1[L] with Serializable {
    type Prefix <: HList
    type Suffix <: HList
    type Out = (Prefix, Suffix)

    def apply(l: L): Out = toTuple2(product(l))
    def product(l: L): Prefix :: Suffix :: HNil
  }

  object ReverseSplit {
    def apply[L <: HList, N <: Nat]
      (implicit split: ReverseSplit[L, N]): Aux[L, N, split.Prefix, split.Suffix] = split

    type Aux[L <: HList, N <: Nat, Prefix0, Suffix0] = ReverseSplit[L, N] {
      type Prefix = Prefix0
      type Suffix = Suffix0
    }

    implicit def reverseSplit[L <: HList, N <: Nat, P <: HList, S <: HList]
      (implicit split : ReverseSplit0[HNil, L, N, P, S]): Aux[L, N, P, S] =
        new ReverseSplit[L, N] {
          type Prefix = P
          type Suffix = S
          def product(l : L): Prefix :: Suffix :: HNil = split(HNil, l)
        }

    trait ReverseSplit0[AccP <: HList, AccS <: HList, N <: Nat, P, S] extends Serializable {
      def apply(accP : AccP, accS : AccS): P :: S :: HNil
    }

    object ReverseSplit0 {
      implicit def hlistReverseSplit1[P <: HList, S <: HList]: ReverseSplit0[P, S, _0, P, S] =
        new ReverseSplit0[P, S, _0, P, S] {
          def apply(accP : P, accS : S): P :: S :: HNil = accP :: accS :: HNil
        }

      implicit def hlistReverseSplit2[AccP <: HList, AccSH, AccST <: HList, N <: Nat, P, S]
        (implicit st : ReverseSplit0[AccSH :: AccP, AccST, N, P, S]): ReverseSplit0[AccP, AccSH :: AccST, Succ[N], P, S] =
          new ReverseSplit0[AccP, AccSH :: AccST, Succ[N], P, S] {
            def apply(accP : AccP, accS : AccSH :: AccST): P :: S :: HNil = st(accS.head :: accP, accS.tail)
          }
    }
  }

  /**
   * Type class supporting splitting this `HList` at the first occurrence of an element of type `U` returning the prefix
   * and suffix as a pair. Available only if this `HList` contains an element of type `U`.
   *
   * @author Miles Sabin
   */
  @implicitNotFound("Implicit not found: shapeless.Ops.SplitLeft[${L}, ${U}]. You requested to split at an element of type ${U}, but there is none in the HList ${L}.")
  trait SplitLeft[L <: HList, U] extends DepFn1[L] with Serializable {
    type Prefix <: HList
    type Suffix <: HList
    type Out = (Prefix, Suffix)

    def apply(l: L): Out = toTuple2(product(l))
    def product(l: L): Prefix :: Suffix :: HNil
  }

  object SplitLeft {
    def apply[L <: HList, U](implicit split: SplitLeft[L, U]): Aux[L, U, split.Prefix, split.Suffix] = split

    type Aux[L <: HList, U, Prefix0 <: HList, Suffix0 <: HList] = SplitLeft[L, U] {
      type Prefix = Prefix0
      type Suffix = Suffix0
    }

    implicit def splitLeft[L <: HList, U, P <: HList, S <: HList]
      (implicit splitLeft : SplitLeft0[HNil, L, U, P, S]): Aux[L, U, P, S] =
        new SplitLeft[L, U] {
          type Prefix = P
          type Suffix = S

          def product(l : L): Prefix :: Suffix :: HNil = splitLeft(HNil, l)
        }

    trait SplitLeft0[AccP <: HList, AccS <: HList, U, P <: HList, S <: HList] extends Serializable {
      def apply(accP : AccP, accS : AccS) : P :: S :: HNil
    }

    trait LowPrioritySplitLeft0 {
      implicit def hlistSplitLeft1[AccP <: HList, AccSH, AccST <: HList, U, P <: HList, S <: HList]
        (implicit slt : SplitLeft0[AccP, AccST, U, P, S]): SplitLeft0[AccP, AccSH :: AccST, U, AccSH :: P, S] =
          new SplitLeft0[AccP, AccSH :: AccST, U, AccSH :: P, S] {
            def apply(accP : AccP, accS : AccSH :: AccST): (AccSH :: P) :: S :: HNil =
              slt(accP, accS.tail) match {
                case prefix :: suffix :: HNil => (accS.head :: prefix) :: suffix :: HNil
              }
          }
    }

    object SplitLeft0 extends LowPrioritySplitLeft0 {
      implicit def hlistSplitLeft2[P <: HList, SH, ST <: HList]: SplitLeft0[P, SH :: ST, SH, P, SH :: ST] =
        new SplitLeft0[P, SH :: ST, SH, P, SH :: ST] {
          def apply(accP : P, accS : SH :: ST) : P :: (SH :: ST) :: HNil = accP :: accS :: HNil
        }
    }
  }

  /**
   * Type class supporting splitting this `HList` at the first occurrence of an element of type `U` returning the reverse
   * prefix and suffix as a pair. Available only if this `HList` contains an element of type `U`.
   *
   * @author Miles Sabin
   */
  @implicitNotFound("Implicit not found: shapeless.Ops.ReverseSplitLeft[${L}, ${U}]. You requested to split at an element of type ${U}, but there is none in the HList ${L}.")
  trait ReverseSplitLeft[L <: HList, U] extends DepFn1[L] with Serializable {
    type Prefix <: HList
    type Suffix <: HList
    type Out = (Prefix, Suffix)

    def apply(l: L): Out = toTuple2(product(l))
    def product(l: L): Prefix :: Suffix :: HNil
  }

  object ReverseSplitLeft {
    def apply[L <: HList, U]
      (implicit split: ReverseSplitLeft[L, U]): Aux[L, U, split.Prefix, split.Suffix] = split

    type Aux[L <: HList, U, Prefix0 <: HList, Suffix0 <: HList] = ReverseSplitLeft[L, U] {
      type Prefix = Prefix0
      type Suffix = Suffix0
    }

    implicit def reverseSplitLeft[L <: HList, U, P <: HList, S <: HList]
      (implicit splitLeft : ReverseSplitLeft0[HNil, L, U, P, S]): Aux[L, U, P, S] =
        new ReverseSplitLeft[L, U] {
          type Prefix = P
          type Suffix = S
          def product(l : L): Prefix :: Suffix :: HNil = splitLeft(HNil, l)
        }

    trait ReverseSplitLeft0[AccP <: HList, AccS <: HList, U, P, S] extends Serializable {
      def apply(accP : AccP, accS : AccS): P :: S :: HNil
    }

    trait LowPriorityReverseSplitLeft0 {
      implicit def hlistReverseSplitLeft1[AccP <: HList, AccSH, AccST <: HList, U, P, S]
        (implicit slt : ReverseSplitLeft0[AccSH :: AccP, AccST, U, P, S]): ReverseSplitLeft0[AccP, AccSH :: AccST, U, P, S] =
          new ReverseSplitLeft0[AccP, AccSH :: AccST, U, P, S] {
            def apply(accP : AccP, accS : AccSH :: AccST): P :: S :: HNil = slt(accS.head :: accP, accS.tail)
          }
    }

    object ReverseSplitLeft0 extends LowPriorityReverseSplitLeft0 {
      implicit def hlistReverseSplitLeft2[P <: HList, SH, ST <: HList]: ReverseSplitLeft0[P, SH :: ST, SH, P, SH :: ST] =
        new ReverseSplitLeft0[P, SH :: ST, SH, P, SH :: ST] {
          def apply(accP : P, accS : SH :: ST) : P :: (SH :: ST) :: HNil = accP :: accS :: HNil
        }
    }
  }

  /**
   * Type class supporting splitting this `HList` at the last occurrence of an element of type `U` returning the prefix
   * and suffix as a pair. Available only if this `HList` contains an element of type `U`.
   *
   * @author Miles Sabin
   */
  @implicitNotFound("Implicit not found: shapeless.Ops.SplitRight[${L}, ${U}]. You requested to split at an element of type ${U}, but there is none in the HList ${L}.")
  trait SplitRight[L <: HList, U] extends DepFn1[L] with Serializable {
    type Prefix <: HList
    type Suffix <: HList
    type Out = (Prefix, Suffix)

    def apply(l: L): Out = toTuple2(product(l))
    def product(l: L): Prefix :: Suffix :: HNil
  }

  object SplitRight {
    def apply[L <: HList, U](implicit split: SplitRight[L, U]): Aux[L, U, split.Prefix, split.Suffix] = split

    type Aux[L <: HList, U, Prefix0 <: HList, Suffix0 <: HList] = SplitRight[L, U] {
      type Prefix = Prefix0
      type Suffix = Suffix0
    }

    implicit def splitRight[L <: HList, U, P <: HList, S <: HList]
      (implicit splitRight : SplitRight0[L, HNil, HNil, U, P, S]): Aux[L, U, P, S] =
        new SplitRight[L, U] {
          type Prefix = P
          type Suffix = S
          def product(l : L): Prefix :: Suffix :: HNil = splitRight(l, HNil, HNil)
        }

    trait SplitRight0[Rev <: HList, AccP <: HList, AccS <: HList, U, P <: HList, S <: HList] extends Serializable {
      def apply(rev : Rev, accP : AccP, accS : AccS): P :: S :: HNil
    }

    trait LowPrioritySplitRight0 {
      implicit def hlistSplitRight1[RevH, RevT <: HList, AccP <: HList, U, P <: HList, S <: HList]
        (implicit srt : SplitRight0[RevT, RevH :: AccP, HNil, U, P, S]): SplitRight0[RevH :: RevT, AccP, HNil, U, P, S] =
          new SplitRight0[RevH :: RevT, AccP, HNil, U, P, S] {
            def apply(rev : RevH :: RevT, accP : AccP, accS : HNil): P :: S :: HNil = srt(rev.tail, rev.head :: accP, accS)
          }

      implicit def hlistSplitRight2[AccPH, AccPT <: HList, AccS <: HList, U, P <: HList, S <: HList]
        (implicit srt : SplitRight0[HNil, AccPT, AccPH :: AccS, U, P, S]): SplitRight0[HNil, AccPH :: AccPT, AccS, U, P, S] =
          new SplitRight0[HNil, AccPH :: AccPT, AccS, U, P, S] {
            def apply(rev : HNil, accP : AccPH :: AccPT, accS : AccS): P :: S :: HNil = srt(rev, accP.tail, accP.head :: accS)
          }
    }

    object SplitRight0 extends LowPrioritySplitRight0 {
      implicit def hlistSplitRight3[PH, PT <: HList, S <: HList, Out0 <: HList]
        (implicit reverse : Reverse.Aux[PH :: PT, Out0]): SplitRight0[HNil, PH :: PT, S, PH, Out0, S] =
          new SplitRight0[HNil, PH :: PT, S, PH, Out0, S] {
            def apply(rev : HNil, accP : PH :: PT, accS : S): Out0 :: S :: HNil = accP.reverse :: accS :: HNil
          }
    }
  }

  /**
   * Type class supporting splitting this `HList` at the last occurrence of an element of type `U` returning the reverse
   * prefix and suffix as a pair. Available only if this `HList` contains an element of type `U`.
   *
   * @author Miles Sabin
   */
  @implicitNotFound("Implicit not found: shapeless.Ops.ReverseSplitRight[${L}, ${U}]. You requested to split at an element of type ${U}, but there is none in the HList ${L}.")
  trait ReverseSplitRight[L <: HList, U] extends DepFn1[L] with Serializable {
    type Prefix <: HList
    type Suffix <: HList
    type Out = (Prefix, Suffix)

    def apply(l: L): Out = toTuple2(product(l))
    def product(l: L): Prefix :: Suffix :: HNil
  }

  object ReverseSplitRight {
    def apply[L <: HList, U](implicit split: ReverseSplitRight[L, U]): Aux[L, U, split.Prefix, split.Suffix] = split

    type Aux[L <: HList, U, Prefix0 <: HList, Suffix0 <: HList] = ReverseSplitRight[L, U] {
      type Prefix = Prefix0
      type Suffix = Suffix0
    }

    implicit def reverseSplitRight[L <: HList, U, P <: HList, S <: HList]
      (implicit splitRight : ReverseSplitRight0[L, HNil, HNil, U, P, S]): Aux[L, U, P, S] =
        new ReverseSplitRight[L, U] {
          type Prefix = P
          type Suffix = S
          def product(l : L): Prefix :: Suffix :: HNil = splitRight(l, HNil, HNil)
        }

    trait ReverseSplitRight0[Rev <: HList, AccP <: HList, AccS <: HList, U, P, S] extends Serializable {
      def apply(rev : Rev, accP : AccP, accS : AccS): P :: S :: HNil
    }

    trait LowPriorityReverseSplitRight0 {
      implicit def hlistReverseSplitRight1[RevH, RevT <: HList, AccP <: HList, U, P <: HList, S <: HList]
        (implicit srt : ReverseSplitRight0[RevT, RevH :: AccP, HNil, U, P, S]): ReverseSplitRight0[RevH :: RevT, AccP, HNil, U, P, S] =
          new ReverseSplitRight0[RevH :: RevT, AccP, HNil, U, P, S] {
            def apply(rev : RevH :: RevT, accP : AccP, accS : HNil): P :: S :: HNil = srt(rev.tail, rev.head :: accP, accS)
          }

      implicit def hlistReverseSplitRight2[AccPH, AccPT <: HList, AccS <: HList, U, P <: HList, S <: HList]
        (implicit srt : ReverseSplitRight0[HNil, AccPT, AccPH :: AccS, U, P, S]): ReverseSplitRight0[HNil, AccPH :: AccPT, AccS, U, P, S] =
          new ReverseSplitRight0[HNil, AccPH :: AccPT, AccS, U, P, S] {
            def apply(rev : HNil, accP : AccPH :: AccPT, accS : AccS): P :: S :: HNil = srt(rev, accP.tail, accP.head :: accS)
          }
    }

    object ReverseSplitRight0 extends LowPriorityReverseSplitRight0 {
      implicit def hlistReverseSplitRight3[PH, PT <: HList, S <: HList]: ReverseSplitRight0[HNil, PH :: PT, S, PH, PH :: PT, S] =
        new ReverseSplitRight0[HNil, PH :: PT, S, PH, PH :: PT, S] {
          def apply(rev : HNil, accP : PH :: PT, accS : S): (PH :: PT) :: S :: HNil = accP :: accS :: HNil
        }
    }
  }

  /**
   * Type class supporting reversing this `HList`.
   *
   * @author Miles Sabin
   */
  trait Reverse[L <: HList] extends DepFn1[L] with Serializable { type Out <: HList }

  object Reverse {
    def apply[L <: HList](implicit reverse: Reverse[L]): Aux[L, reverse.Out] = reverse

    type Aux[L <: HList, Out0 <: HList] = Reverse[L] { type Out = Out0 }

    implicit def reverse[L <: HList, Out0 <: HList](implicit reverse : Reverse0[HNil, L, Out0]): Aux[L, Out0] =
      new Reverse[L] {
        type Out = Out0
        def apply(l : L) : Out = reverse(HNil, l)
      }

    trait Reverse0[Acc <: HList, L <: HList, Out <: HList] extends Serializable {
      def apply(acc : Acc, l : L) : Out
    }

    object Reverse0 {
      implicit def hnilReverse[Out <: HList]: Reverse0[Out, HNil, Out] =
        new Reverse0[Out, HNil, Out] {
          def apply(acc : Out, l : HNil) : Out = acc
        }

      implicit def hlistReverse[Acc <: HList, InH, InT <: HList, Out <: HList]
        (implicit rt : Reverse0[InH :: Acc, InT, Out]): Reverse0[Acc, InH :: InT, Out] =
          new Reverse0[Acc, InH :: InT, Out] {
            def apply(acc : Acc, l : InH :: InT) : Out = rt(l.head :: acc, l.tail)
          }
    }
  }

  /**
   * Type class supporting permuting this `HList` into the same order as another `HList` with
   * the same element types.
   *
   * @author Miles Sabin
   */
  trait Align[L <: HList, M <: HList] extends (L => M) with Serializable {
    def apply(l: L): M
  }

  object Align {
    def apply[L <: HList, M <: HList](implicit alm: Align[L, M]): Align[L, M] = alm

    implicit val hnilAlign: Align[HNil, HNil] = new Align[HNil, HNil] {
      def apply(l: HNil): HNil = l
    }

    implicit def hlistAlign[L <: HList, MH, MT <: HList, R <: HList]
      (implicit select: Remove.Aux[L, MH, (MH, R)], alignTail: Align[R, MT]): Align[L, MH :: MT] = new Align[L, MH :: MT] {
      def apply(l: L): MH :: MT = {
        val (h, t) = l.removeElem[MH]
        h :: alignTail(t)
      }
    }
  }

  /**
   * Type class supporting prepending to this `HList`.
   *
   * @author Miles Sabin
   */
  trait Prepend[P <: HList, S <: HList] extends DepFn2[P, S] with Serializable { type Out <: HList }

  trait LowestPriorityPrepend {
    type Aux[P <: HList, S <: HList, Out0 <: HList] = Prepend[P, S] { type Out = Out0 }

    implicit def hlistPrepend[PH, PT <: HList, S <: HList, PtOut <: HList]
     (implicit pt : Prepend.Aux[PT, S, PtOut]): Prepend.Aux[PH :: PT, S, PH :: PtOut] =
      new Prepend[PH :: PT, S] {
        type Out = PH :: PtOut
        def apply(prefix : PH :: PT, suffix : S): Out = prefix.head :: pt(prefix.tail, suffix)
      }
  }

  trait LowPriorityPrepend extends LowestPriorityPrepend {
    /**
     * Binary compatibility stub
     * This one is for https://github.com/milessabin/shapeless/issues/406
     */
    override type Aux[P <: HList, S <: HList, Out0 <: HList] = Prepend[P, S] { type Out = Out0 }

    implicit def hnilPrepend0[P <: HList, S <: HNil]: Aux[P, S, P] =
      new Prepend[P, S] {
        type Out = P
        def apply(prefix : P, suffix : S): P = prefix
      }
  }

  object Prepend extends LowPriorityPrepend {
    def apply[P <: HList, S <: HList](implicit prepend: Prepend[P, S]): Aux[P, S, prepend.Out] = prepend

    implicit def hnilPrepend1[P <: HNil, S <: HList]: Aux[P, S, S] =
      new Prepend[P, S] {
        type Out = S
        def apply(prefix : P, suffix : S): S = suffix
      }
  }

  /**
   * Type class supporting reverse prepending to this `HList`.
   *
   * @author Miles Sabin
   */
  trait ReversePrepend[P <: HList, S <: HList] extends DepFn2[P, S] with Serializable { type Out <: HList }

  trait LowPriorityReversePrepend {
    type Aux[P <: HList, S <: HList, Out0 <: HList] = ReversePrepend[P, S] { type Out = Out0 }

    implicit def hnilReversePrepend0[P <: HList, S <: HNil]
      (implicit rv: Reverse[P]): Aux[P, S, rv.Out] =
        new ReversePrepend[P, S] {
          type Out = rv.Out
          def apply(prefix: P, suffix: S) = prefix.reverse
        }
  }

  object ReversePrepend extends LowPriorityReversePrepend {
    def apply[P <: HList, S <: HList](implicit prepend: ReversePrepend[P, S]): Aux[P, S, prepend.Out] = prepend

    implicit def hnilReversePrepend1[P <: HNil, S <: HList]: Aux[P, S, S] =
      new ReversePrepend[P, S] {
        type Out = S
        def apply(prefix: P, suffix: S) = suffix
      }

    implicit def hlistReversePrepend[PH, PT <: HList, S <: HList]
      (implicit rpt : ReversePrepend[PT, PH :: S]): Aux[PH :: PT, S, rpt.Out] =
        new ReversePrepend[PH :: PT, S] {
          type Out = rpt.Out
          def apply(prefix : PH :: PT, suffix : S): Out = rpt(prefix.tail, prefix.head :: suffix)
        }
  }

  /**
    * Typeclass supporting repeating L-typed HLists N times.
    *
    * Repeat[Int :: String :: HNil, Nat._2] => Int :: String :: Int :: String :: HNil
    *
    * @author Jeremy Smith
    */
  trait Repeat[L <: HList, N <: Nat] extends RepeatVersionSpecific[L]

  object Repeat {

    type Aux[L <: HList, N <: Nat, Out0 <: HList] = Repeat[L, N] { type Out = Out0 }

    def apply[L <: HList, N <: Nat](implicit repeat: Repeat[L, N]): Aux[L, N, repeat.Out] = repeat

    implicit def base[L <: HList]: Aux[L, Nat._1, L] = new Repeat[L, Nat._1] {
      type Out = L
      def apply(l: L): L = l
    }

    implicit def succ[L <: HList, Prev <: Nat, PrevOut <: HList, P <: HList](implicit
      prev: Aux[L, Prev, PrevOut],
      prepend: Prepend.Aux[L, PrevOut, P]
    ): Aux[L, Succ[Prev], P] = new Repeat[L, Succ[Prev]] {
      type Out = P
      def apply(l: L): P = prepend(l, prev(l))
    }

  }

  /**
   * Type class supporting zipping this `HList` with an `HList` of `HList`s returning an `HList` of `HList`s with each
   * element of this `HList` prepended to the corresponding `HList` element of the argument `HList`.
   *
   * @author Miles Sabin
   */
  trait ZipOne[H <: HList, T <: HList] extends DepFn2[H, T] with Serializable { type Out <: HList }

  object ZipOne extends LowPriorityZipOne {
    implicit def zipOne0: Aux[HNil, HNil, HNil] =
      new ZipOne[HNil, HNil] {
        type Out = HNil
        def apply(h : HNil, t : HNil): Out = HNil
      }

    implicit def zipOne3[H, T <: HList]: Aux[H :: HNil, T :: HNil, (H :: T) :: HNil] =
      new ZipOne[H :: HNil, T :: HNil] {
        type Out = (H :: T) :: HNil
        def apply(h : H :: HNil, t : T :: HNil): Out = (h.head :: t.head) :: HNil
      }
  }

  trait LowPriorityZipOne {
    def apply[H <: HList, T <: HList](implicit zip: ZipOne[H, T]): Aux[H, T, zip.Out] = zip

    type Aux[H <: HList, T <: HList, Out0 <: HList] = ZipOne[H, T] { type Out = Out0 }

    implicit def zipOne1[H <: HList]: Aux[H, HNil, HNil] =
      new ZipOne[H, HNil] {
        type Out = HNil
        def apply(h : H, t : HNil): Out = HNil
      }

    implicit def zipOne2[T <: HList]: Aux[HNil, T, HNil] =
      new ZipOne[HNil, T] {
        type Out = HNil
        def apply(h : HNil, t : T): Out = HNil
      }

    implicit def zipOne4[HH, HT <: HList, TH <: HList, TT <: HList, ZotOut <: HList]
    (implicit zot : ZipOne.Aux[HT, TT, ZotOut]): Aux[HH :: HT, TH :: TT, (HH :: TH) :: ZotOut] =
        new ZipOne[HH :: HT, TH :: TT] {
          type Out = (HH :: TH) :: ZotOut
          def apply(h : HH :: HT, t : TH :: TT): Out = (h.head :: t.head) :: zot(h.tail, t.tail)
        }
  }

  /**
   * Type class supporting transposing this `HList`.
   *
   * @author Miles Sabin
   */
  trait Transposer[L <: HList] extends DepFn1[L] with Serializable { type Out <: HList }

  object Transposer {
    def apply[L <: HList](implicit transposer: Transposer[L]): Aux[L, transposer.Out] = transposer

    type Aux[L <: HList, Out0 <: HList] = Transposer[L] { type Out = Out0 }

    implicit def hnilTransposer: Aux[HNil, HNil] =
      new Transposer[HNil] {
        type Out = HNil
        def apply(l : HNil): Out = l
      }

    implicit def hlistTransposer1[H <: HList, MC <: HList, Out0 <: HList]
      (implicit mc : ConstMapper.Aux[HNil, H, MC], zo : ZipOne.Aux[H, MC, Out0]): Aux[H :: HNil, Out0] =
        new Transposer[H :: HNil] {
          type Out = Out0
          def apply(l : H :: HNil): Out = zo(l.head, mc(HNil, l.head))
        }

    implicit def hlistTransposer2[H <: HList, TH <: HList, TT <: HList, OutT <: HList, Out0 <: HList]
      (implicit tt : Aux[TH :: TT, OutT], zo : ZipOne.Aux[H, OutT, Out0]): Aux[H :: TH :: TT, Out0] =
        new Transposer[H :: TH :: TT] {
          type Out = Out0
          def apply(l : H :: TH :: TT): Out = zo(l.head, tt(l.tail))
        }
  }

  /**
   * Type class supporting zipping this `HList` of `HList`s returning an `HList` of tuples.
   *
   * @author Miles Sabin
   */
  trait Zip[L <: HList] extends DepFn1[L] with Serializable { type Out <: HList }

  object Zip {
    def apply[L <: HList](implicit zip: Zip[L]): Aux[L, zip.Out] = zip

    type Aux[L <: HList, Out0 <: HList] = Zip[L] { type Out = Out0 }

    implicit def zipper[L <: HList, OutT <: HList, OutM <: HList]
      (implicit
        transposer : Transposer.Aux[L, OutT],
        mapper : Mapper.Aux[tupled.type, OutT, OutM]): Aux[L, OutM] =
        new Zip[L] {
          type Out = OutM
          def apply(l : L): Out = l.transpose map tupled
        }
  }

  /**
   * Type class supporting unzipping this `HList` of tuples returning a tuple of `HList`s.
   *
   * @author Miles Sabin
   */
  trait Unzip[L <: HList] extends DepFn1[L] with Serializable

  object Unzip {
    def apply[L <: HList](implicit unzip: Unzip[L]): Aux[L, unzip.Out] = unzip

    type Aux[L <: HList, Out0] = Unzip[L] { type Out = Out0 }

    implicit def unzipper[L <: HList, OutM <: HList, TransposerOut <: HList, TuplerOut]
      (implicit
        mapper : Mapper.Aux[productElements.type, L, OutM],
        transposer : Transposer.Aux[OutM, TransposerOut],
        tupler : Tupler.Aux[TransposerOut, TuplerOut]): Aux[L, TuplerOut] =
        new Unzip[L] {
          type Out = TuplerOut
          def apply(l : L): Out = (l map productElements).transpose.tupled
        }
  }

  /**
   * Type class supporting zipping this `HList` of monomorphic function values with its argument `HList` of
   * correspondingly typed function arguments returning the result of each application as an `HList`. Available only if
   * there is evidence that the corresponding function and argument elements have compatible types.
   *
   * @author Miles Sabin
   */
  @implicitNotFound("Implicit not found: shapeless.Ops.ZipApply[${FL}, ${AL}]. The types of ${FL} and ${AL} are not compatible.")
  trait ZipApply[FL <: HList, AL <: HList] extends DepFn2[FL, AL] with Serializable { type Out <: HList }

  object ZipApply {
    def apply[FL <: HList, AL <: HList](implicit zip: ZipApply[FL, AL]): Aux[FL, AL, zip.Out] = zip

    type Aux[FL <: HList, AL <: HList, Out0 <: HList] = ZipApply[FL, AL] { type Out = Out0 }

    implicit def hnilZipApply: Aux[HNil, HNil, HNil] =
      new ZipApply[HNil, HNil] {
        type Out = HNil
        def apply(fl : HNil, al : HNil): Out = HNil
      }

    implicit def hconsZipApply[T, R, FLT <: HList, ALT <: HList, ZttOut <: HList]
      (implicit ztt : ZipApply.Aux[FLT, ALT, ZttOut]): Aux[(T => R) :: FLT, T :: ALT, R :: ZttOut] =
        new ZipApply[(T => R) :: FLT, T :: ALT] {
          type Out = R :: ZttOut
          def apply(fl : (T => R) :: FLT, al : T :: ALT): Out = fl.head(al.head) :: ztt(fl.tail, al.tail)
        }
  }

  /**
   * Type class supporting zipping an `HList` with a constant, resulting in an `HList` of tuples of the form
   * ({element from input `HList`}, {supplied constant})
   *
   * @author Cody Allen
   */
  trait ZipConst[C, L <: HList] extends DepFn2[C, L] with Serializable { type Out <: HList }

  object ZipConst {
    def apply[C, L <: HList](implicit zip: ZipConst[C, L]): Aux[C, L, zip.Out] = zip

    type Aux[C, L <: HList, Out0 <: HList] = ZipConst[C, L] { type Out = Out0 }

    implicit def hnilZipConst[C, L <: HNil] : Aux[C, L, HNil] = new ZipConst[C, L] {
      type Out = HNil
      def apply(t: C, u: L) = HNil
    }

    implicit def constZipper[C, L <: HList, M <: HList, ZipperOut <: HList]
      (implicit
        mapper: ConstMapper.Aux[C, L, M],
        zipper: Zip.Aux[L :: M :: HNil, ZipperOut]): Aux[C, L, ZipperOut] =
        new ZipConst[C, L] {
          type Out = ZipperOut
          def apply(c: C, l: L) = zipper(l :: mapper(c, l) :: HNil)
        }
  }

  /**
   * Type class supporting zipping an 'HList' with another 'HList' using a 'Poly2' resulting in an HList
   *
   * @author Stacy Curl
   */
  trait ZipWith[L <: HList, R <: HList, P <: Poly2] extends DepFn2[L, R] with Serializable { type Out <: HList }

  object ZipWith {
    def apply[L <: HList, R <: HList, P <: Poly2]
      (implicit zipWith: ZipWith[L, R, P]): Aux[L, R, P, zipWith.Out] = zipWith

    type Aux[L <: HList, R <: HList, P <: Poly2, Out0 <: HList] = ZipWith[L, R, P] { type Out = Out0 }

    implicit def hnilZipWithHNil[P <: Poly2]: Aux[HNil, HNil, P, HNil] = constZipWith[HNil, HNil, P]
    implicit def hnilZipWithHList[R <: HList, P <: Poly2]: Aux[HNil, R, P, HNil] = constZipWith[HNil, R, P]
    implicit def hlistZipWithHNil[L <: HList, P <: Poly2]: Aux[L, HNil, P, HNil] = constZipWith[L, HNil, P]

    implicit def hlistZipWithHList[LH, RH, LT <: HList, RT <: HList, P <: Poly2, ZipWithOut <: HList, ClrResult]
      (implicit zipWith: ZipWith.Aux[LT, RT, P, ZipWithOut], clr: Case2.Aux[P, LH, RH, ClrResult])
        : Aux[LH :: LT, RH :: RT, P, ClrResult :: ZipWithOut] =
          new ZipWith[LH :: LT, RH :: RT, P] {
            type Out = ClrResult :: ZipWithOut
            def apply(l: LH :: LT, r: RH :: RT): Out =
              clr(l.head, r.head) :: zipWith(l.tail, r.tail)
          }

    private def constZipWith[L <: HList, R <: HList, P <: Poly2]: Aux[L, R, P, HNil] =
      new ZipWith[L, R, P] {
        type Out = HNil
        def apply(l: L, r: R): HNil = HNil
      }
  }

  /**
   * Type class supporting zipping an `HList` of values with an `HList` of keys to create a record.
   *
   * @author Cody Allen
   */
  trait ZipWithKeys[K <: HList, V <: HList] extends DepFn1[V] with Serializable { type Out <: HList }

  object ZipWithKeys {
    import shapeless.labelled._

    def apply[K <: HList, V <: HList]
      (implicit zipWithKeys: ZipWithKeys[K, V]): Aux[K, V, zipWithKeys.Out] = zipWithKeys

    type Aux[K <: HList, V <: HList, Out0 <: HList] = ZipWithKeys[K, V] { type Out = Out0 }

    implicit val hnilZipWithKeys: Aux[HNil, HNil, HNil] = new ZipWithKeys[HNil, HNil] {
      type Out = HNil
      def apply(v: HNil) = HNil
    }

    implicit def hconsZipWithKeys[KH, VH, KT <: HList, VT <: HList, ZwkOut <: HList] (implicit zipWithKeys: ZipWithKeys.Aux[KT, VT, ZwkOut], wkh: Witness.Aux[KH])
        : Aux[KH :: KT, VH :: VT, FieldType[KH, VH] :: ZwkOut] =
          new ZipWithKeys[KH :: KT, VH :: VT] {
            type Out = FieldType[KH, VH] :: ZwkOut
            def apply(v: VH :: VT): Out =
              field[wkh.T](v.head) :: zipWithKeys(v.tail)
          }
  }

  /**
   * Type class supporting zipping a `HList` with its element indices,  resulting in a 'HList' of  tuples of the form
   * ({element from input tuple}, {element index})
   *
   * @author Andreas Koestler
   * @author Alexandre Archambault
   */
  trait ZipWithIndex[L <: HList] extends DepFn1[L] with Serializable {
    type Out <: HList
  }

  object ZipWithIndex {
    def apply[L <: HList](implicit zipWithIndex: ZipWithIndex[L]): Aux[L, zipWithIndex.Out] =
      zipWithIndex

    type Aux[L <: HList, Out0 <: HList] = ZipWithIndex[L] { type Out = Out0 }


    trait Helper[L <: HList, N <: Nat] extends DepFn2[L, N] with Serializable {
      type Out <: HList
    }

    object Helper {
      def apply[L <: HList, N <: Nat](implicit helper: Helper[L, N]): Aux[L, N, helper.Out] = helper

      type Aux[L <: HList, N <: Nat, Out0 <: HList] = Helper[L, N] { type Out = Out0 }

      implicit def hnil[N <: Nat]: Aux[HNil, N, HNil] =
        new Helper[HNil, N] {
          type Out = HNil
          def apply(l: HNil, n: N) = HNil
        }

      implicit def hcons[H, T <: HList, OutT <: HList, N <: Nat]
       (implicit
         tail: Helper.Aux[T, Succ[N], OutT]
       ): Aux[H :: T, N, (H, N) :: OutT] =
        new Helper[H :: T, N] {
          type Out = (H, N) :: OutT
          def apply(l: H :: T, n: N) = (l.head -> n) :: tail(l.tail, Succ[N]())
        }
    }


    implicit def default[L <: HList, P <: HList]
     (implicit helper: Helper.Aux[L, _0, P]): Aux[L, P] =
      new ZipWithIndex[L] {
        type Out = P
        def apply(l: L) = helper(l, Nat._0)
      }
  }

  /**
   * Type Class witnessing that an 'HList' can be collected with a 'Poly' to produce an 'HList'
   *
   * @author Stacy Curl
   */
  trait Collect[I <: HList, P <: Poly] extends DepFn1[I] with Serializable { type Out <: HList }

  object Collect extends LowPriorityCollect {
    def apply[L <: HList, P <: Poly]
      (implicit collect: Collect[L, P]): Aux[L, P, collect.Out] = collect

    type Aux[L <: HList, P <: Poly, Out0 <: HList] = Collect[L, P] { type Out = Out0 }

    implicit def hnilCollect[P <: Poly]: Aux[HNil, P, HNil] = new Collect[HNil, P] {
      type Out = HNil

      def apply(l: HNil): Out = HNil
    }

    implicit def hlistCollect[LH, LT <: HList, P <: Poly, CollectOut <: HList, ClrResult]
      (implicit collect: Collect.Aux[LT, P, CollectOut], clr: Case1.Aux[P, LH, ClrResult])
        : Aux[LH :: LT, P, ClrResult :: CollectOut] =
          new Collect[LH :: LT, P] {
            type Out = ClrResult :: CollectOut

            def apply(l: LH :: LT): Out = clr(l.head) :: collect(l.tail)
          }
  }

  trait LowPriorityCollect {
    implicit def hlistNoPolyCase[LH, LT <: HList, P <: Poly, CollectOut <: HList]
      (implicit collect: Collect.Aux[LT, P, CollectOut]): Collect.Aux[LH :: LT, P, CollectOut] =
        new Collect[LH :: LT, P] {
          type Out = CollectOut

          def apply(l: LH :: LT): Out = collect(l.tail)
        }
  }

  implicit object hnilOrdering extends Ordering[HNil] {
    def compare(x: HNil, y: HNil): Int = 0
  }

  implicit def hlistOrdering[H, T <: HList]
    (implicit hOrdering: Ordering[H], tOrdering: Ordering[T]): Ordering[H :: T] =
      new Ordering[H :: T] {
        def compare(x: H :: T, y: H :: T): Int = {
          val compareH = hOrdering.compare(x.head, y.head)

          if (compareH != 0) compareH else tOrdering.compare(x.tail, y.tail)
        }
      }

  /*
   * Type class supporting consing an element onto each row of this HMatrix (HList of HLists)
   *
   * @author Stacy Curl
   */
  trait MapCons[A, M <: HList] extends DepFn2[A, M] with Serializable { type Out <: HList }

  object MapCons {
    def apply[A, M <: HList](implicit mapCons: MapCons[A, M]): Aux[A, M, mapCons.Out] = mapCons

    type Aux[A, M <: HList, Out0 <: HList] = MapCons[A, M] { type Out = Out0 }

    implicit def hnilMapCons[A]: Aux[A, HNil, HNil] =
      new MapCons[A, HNil] {
        type Out = HNil

        def apply(a: A, m: HNil): Out = HNil
      }

    implicit def hlistMapCons[A, H <: HList, TM <: HList, MapConsOut <: HList]
      (implicit mapCons: MapCons.Aux[A, TM, MapConsOut]): Aux[A, H :: TM, (A :: H) :: MapConsOut] =
        new MapCons[A, H :: TM] {
          type Out = (A :: H) :: MapConsOut

          def apply(a: A, l: H :: TM): Out = (a :: l.head) :: mapCons(a, l.tail)
        }
  }

  /**
   * Type class supporting adding an element to each possible position in this HList
   *
   * @author Stacy Curl
   */
  trait Interleave[A, L <: HList] extends DepFn2[A, L] with Serializable { type Out <: HList }

  object Interleave {
    def apply[A , L <: HList](implicit interleave: Interleave[A, L]): Aux[A, L, interleave.Out] = interleave

    type Aux[A, L <: HList, Out0 <: HList] = Interleave[A, L] { type Out = Out0 }

    implicit def hnilInterleave[A, L <: HNil]: Aux[A, L, (A :: HNil) :: HNil] =
      new Interleave[A, L] {
        type Out = (A :: HNil) :: HNil

        def apply(a: A, l: L): Out = (a :: HNil) :: HNil
      }

    implicit def hlistInterleave[A, H, T <: HList, LI <: HList, MapConsOut <: HList]
      (implicit interleave: Interleave.Aux[A, T, LI], mapCons: MapCons.Aux[H, LI, MapConsOut])
        : Aux[A, H :: T, (A :: H :: T) :: MapConsOut] = new Interleave[A, H :: T] {
          type Out = (A :: H :: T) :: MapConsOut

          def apply(a: A, l: H :: T): Out = (a :: l) :: mapCons(l.head, interleave(a, l.tail))
        }
  }

  /**
   * Type class supporting interleaving an element into each row of this HMatrix (HList of HLists)
   *
   * @author Stacy Curl
   */
  trait FlatMapInterleave[A, M <: HList] extends DepFn2[A, M] with Serializable { type Out <: HList }

  object FlatMapInterleave {
    def apply[A, M <: HList]
      (implicit flatMapInterleave: FlatMapInterleave[A, M]): Aux[A, M, flatMapInterleave.Out] = flatMapInterleave

    type Aux[A, M <: HList, Out0 <: HList] = FlatMapInterleave[A, M] { type Out = Out0 }

    implicit def hnilFlatMapInterleave[A, M <: HNil]: Aux[A, M, HNil]
      = new FlatMapInterleave[A, M] {
        type Out = HNil

        def apply(a: A, m: M): Out = HNil
      }

    implicit def hlistFlatMapInterleave[A, H <: HList, TM <: HList, HO <: HList, TMO <: HList, PrependOut <: HList]
      (implicit interleave: Interleave.Aux[A, H, HO],
         flatMapInterleave: FlatMapInterleave.Aux[A, TM, TMO],
         prepend: Prepend.Aux[HO, TMO, PrependOut]
       ): Aux[A, H :: TM, PrependOut] =
          new FlatMapInterleave[A, H :: TM] {
            type Out = PrependOut

            def apply(a: A, m: H :: TM): Out =
              prepend(interleave(a, m.head), flatMapInterleave(a, m.tail))
          }
  }

  /**
   * Type class supporting the calculation of every permutation of this 'HList'
   *
   * @author Stacy Curl
   */
  trait Permutations[L <: HList] extends DepFn1[L] with Serializable { type Out <: HList }

  object Permutations {
    def apply[L <: HList](implicit permutations: Permutations[L]): Aux[L, permutations.Out] = permutations

    type Aux[L <: HList, Out0] = Permutations[L] { type Out = Out0 }

    implicit def hnilPermutations[L <: HNil]: Aux[L, HNil :: HNil] = new Permutations[L] {
      type Out = HNil :: HNil

      def apply(l: L): Out = HNil :: HNil
    }

    implicit def hlistPermutations[H, T <: HList, TP <: HList, FlatmapInterleaveOut <: HList]
      (implicit permutations: Permutations.Aux[T, TP], flatMapInterleave: FlatMapInterleave.Aux[H, TP, FlatmapInterleaveOut])
        : Aux[H :: T, FlatmapInterleaveOut] =
          new Permutations[H :: T] {
            type Out = FlatmapInterleaveOut

            def apply(l: H :: T): Out = flatMapInterleave(l.head, permutations(l.tail))
          }
  }

  /**
   * Type class supporting rotating a HList left
   *
   * @author Stacy Curl
   */
  trait RotateLeft[L <: HList, N <: Nat] extends DepFn1[L] with Serializable { type Out <: HList }

  object RotateLeft extends LowPriorityRotateLeft {
    def apply[L <: HList, N <: Nat]
      (implicit rotateLeft: RotateLeft[L, N]): Aux[L, N, rotateLeft.Out] = rotateLeft

    implicit def hnilRotateLeft[L <: HNil, N <: Nat]: RotateLeft.Aux[L, N, L] = new RotateLeft[L, N] {
      type Out = L
      def apply(l: L) = l
    }
  }

  trait LowPriorityRotateLeft {
    type Aux[L <: HList, N <: Nat, Out0] = RotateLeft[L, N] { type Out = Out0 }

    implicit def hlistRotateLeft[
    L <: HList, N <: Nat, Size <: Nat, NModSize <: Nat, Before <: HList, After <: HList, PrependOut <: HList
    ](implicit
      length: Length.Aux[L, Size],
      mod: nat.Mod.Aux[N, Size, NModSize],
      split: Split.Aux[L, NModSize, Before, After],
      prepend: Prepend.Aux[After, Before, PrependOut]
    ): RotateLeft.Aux[L, N, PrependOut] = new RotateLeft[L, N] {
      type Out = PrependOut

      def apply(l: L): Out = {
        val (before, after) = split(l)

        prepend(after, before)
      }
    }

    /** Binary compatibility stub */
    def noopRotateLeft[L <: HList, N <: Nat]: Aux[L, N, L] = new RotateLeft[L, N] {
      type Out = L
      def apply(l: L): Out = l
    }
  }

  /**
   * Type class supporting rotating a HList right
   *
   * @author Stacy Curl
   */
  trait RotateRight[L <: HList, N <: Nat] extends DepFn1[L] with Serializable { type Out <: HList }

  object RotateRight extends LowPriorityRotateRight {
    def apply[L <: HList, N <: Nat]
      (implicit rotateRight: RotateRight[L, N]): Aux[L, N, rotateRight.Out] = rotateRight

    implicit def hnilRotateRight[L <: HNil, N <: Nat]: RotateRight.Aux[L, N, L] = new RotateRight[L, N] {
      type Out = L
      def apply(l: L) = l
    }
  }

  trait LowPriorityRotateRight {
    type Aux[L <: HList, N <: Nat, Out0 <: HList] = RotateRight[L, N] { type Out = Out0 }

    implicit def hlistRotateRight[
    L <: HList, N <: Nat, Size <: Nat, NModSize <: Nat, Size_Diff_NModSize <: Nat, RotateLeftOut <: HList
    ](implicit
      length: Length.Aux[L, Size],
      mod: nat.Mod.Aux[N, Size, NModSize],
      diff: nat.Diff.Aux[Size, NModSize, Size_Diff_NModSize],
      rotateLeft: RotateLeft.Aux[L, Size_Diff_NModSize, RotateLeftOut]
       ): RotateRight.Aux[L, N, RotateLeftOut] = new RotateRight[L, N] {
      type Out = RotateLeftOut

      def apply(l: L): Out = rotateLeft(l)
    }

    def noopRotateRight[L <: HList, N <: Nat]: Aux[L, N, L] = new RotateRight[L, N] {
      type Out = L
      def apply(l: L): Out = l
    }
  }

  /**
   * Type class supporting left scanning of this `HList` with a binary polymorphic function.
   *
   * @author Owein Reese
   */
  trait LeftScanner[L <: HList, In, P <: Poly] extends DepFn2[L, In] with Serializable {
    type Out <: HList
  }

  object LeftScanner{
    def apply[L <: HList, In, P <: Poly](implicit scan: LeftScanner[L, In, P]): Aux[L, In, P, scan.Out] = scan

    type Aux[L <: HList, In, P <: Poly, Out0 <: HList] = LeftScanner[L, In, P]{ type Out = Out0 }

    implicit def hnilLeftScanner[In, P <: Poly]: Aux[HNil, In, P, In :: HNil] =
      new LeftScanner[HNil, In, P]{
        type Out = In :: HNil

        def apply(l: HNil, in: In) = in :: HNil
      }

    implicit def hlistLeftScanner[H, T <: HList, In, P <: Poly, OutP, ScanOut <: HList]
      (implicit ev: Case2.Aux[P, H, In, OutP], scan: LeftScanner.Aux[T, OutP, P, ScanOut]): Aux[H :: T, In, P, In :: ScanOut] =
        new LeftScanner[H :: T, In, P]{
          type Out = In :: ScanOut

          def apply(l: H :: T, in: In) = in :: scan(l.tail, ev(l.head, in)) // check it's (h, in) vs (in, h)
        }
  }

  /**
   * Type class supporting right scanning of this `HList` with a binary polymorphic function.
   *
   * @author Owein Reese
   */
  trait RightScanner[L <: HList, In, P <: Poly] extends DepFn2[L, In] with Serializable {
    type Out <: HList
  }

  object RightScanner {

    type Aux[L <: HList, In, P <: Poly, Out0 <: HList] = RightScanner[L, In, P] { type Out = Out0 }

    def apply[L <: HList, In, P <: Poly](implicit scanR: RightScanner[L, In, P]): Aux[L, In, P, scanR.Out] = scanR

    trait RightScanner0[L <: HList, V, P <: Poly] extends DepFn2[L, V] with Serializable {
      type Out <: HList
    }

    object RightScanner0 {
      type Aux[L <: HList, V, P <: Poly, Out0 <: HList] = RightScanner0[L, V, P] { type Out = Out0 }
    }

    implicit def hlistRightScanner0[H, H0, T <: HList, P <: Poly, C2Result](implicit ev: Case2.Aux[P, H0, H, C2Result]) =
      new RightScanner0[H :: T, H0, P]{
        type Out = C2Result :: H :: T

        def apply(l: H :: T, h: H0) = ev(h, l.head) :: l
      }

    implicit def hnilRightScanner[In, P <: Poly]: Aux[HNil, In, P, In :: HNil] =
      new RightScanner[HNil, In, P]{
        type Out = In :: HNil

        def apply(l: HNil, in: In): Out = in :: HNil
      }

    implicit def hlistRightScanner[H, T <: HList, In, P <: Poly, R <: HList, Scan0Out <: HList]
      (implicit scanR: Aux[T, In, P, R], scan0: RightScanner0.Aux[R, H, P, Scan0Out]): RightScanner.Aux[H :: T, In, P, Scan0Out] =
        new RightScanner[H :: T, In, P]{
          type Out = Scan0Out

          def apply(l: H :: T, in: In) = scan0(scanR(l.tail, in), l.head)
        }
  }

  /**
   * Type class supporting producing a HList of shape `N` filled with elements of type `A`.
   *
   * @author Alexandre Archambault
   */
  trait Fill[N, A] extends DepFn1[A] with Serializable { type Out <: HList }

  object Fill {
    def apply[N, A](implicit fill: Fill[N, A]): Aux[N, A, fill.Out] = fill

    type Aux[N, A, Out0] = Fill[N, A] { type Out = Out0 }

    implicit def fill1Zero[A]: Aux[Nat._0, A, HNil] =
      new Fill[Nat._0, A] {
        type Out = HNil
        def apply(elem: A) = HNil
      }

    implicit def fill1Succ[N <: Nat, A, OutT <: HList]
      (implicit prev: Aux[N, A, OutT]): Aux[Succ[N], A, A :: OutT] =
        new Fill[Succ[N], A] {
          type Out = A :: OutT
          def apply(elem: A) = elem :: prev(elem)
        }

    implicit def fill2[A, N1 <: Nat, N2 <: Nat, SubOut, OutT <: HList]
      (implicit subFill: Aux[N2, A, SubOut], fill: Aux[N1, SubOut, OutT]): Aux[(N1, N2), A, OutT] =
        new Fill[(N1, N2), A] {
          type Out = OutT
          def apply(elem: A) = fill(subFill(elem))
        }
  }

  /**
    * Type class supporting producing a [[HList]] filled from a [[Poly0]].
    *
    * @author 杨博 (Yang Bo)
    */
  trait FillWith[F, L <: HList] extends DepFn0 with Serializable {
    type Out = L
  }

  object FillWith {
    def apply[F, L <: HList](implicit fill: FillWith[F, L]): FillWith[F, L] = fill

    implicit def hnilFill[F]: FillWith[F, HNil] =
      new FillWith[F, HNil] {
        def apply(): Out = HNil
      }

    implicit def hconsFill[F <: Poly, Head, Tail <: HList]
    (implicit hc: Case0.Aux[F, Head], mt: FillWith[F, Tail]): FillWith[F, Head :: Tail] =
      new FillWith[F, Head :: Tail] {
        def apply(): Out = hc() :: mt()
      }
  }

  /**
   * Type class supporting the patching of an `HList`
   *
   * @author Owein Reese
   */
  trait Patcher[N <: Nat, M <: Nat, L <: HList, In <: HList] extends DepFn2[L, In] with Serializable {
    type Out <: HList
  }

  object Patcher {
    def apply[N <: Nat, M <: Nat, L <: HList, In <: HList](
      implicit patch: Patcher[N, M, L, In]
    ): Aux[N, M, L, In, patch.Out] = patch

    type Aux[N <: Nat, M <: Nat, L <: HList, In <: HList, Out0 <: HList] = Patcher[N, M, L, In]{ type Out = Out0 }

    implicit def hlistPatch1[N <: Nat, M <: Nat, H, T <: HList, In <: HList, PatchOut <: HList]
      (implicit patch: Patcher.Aux[N, M, T, In, PatchOut]): Aux[Succ[N], M, H :: T, In, H :: PatchOut] =
        new Patcher[Succ[N], M, H :: T, In]{
          type Out = H :: PatchOut

          def apply(l: H :: T, in: In) = l.head :: patch(l.tail, in)
        }

    implicit def hlistPatch2[M <: Nat, L <: HList, In <: HList, OutL <: HList, OutP <: HList]
      (implicit drop: Drop.Aux[L, M, OutL], prepend: Prepend.Aux[In, OutL, OutP]) =
        new Patcher[_0, M, L, In]{
          type Out = OutP

          def apply(l: L, in: In) = prepend(in, drop(l))
        }
  }

  /**
   * Type class witnessing that there exists at least one element of an `HList` for which a `Poly` can be
   * applied.
   *
   * @author Owein Reese
   */
  trait CollectFirst[L <: HList, P <: Poly] extends DepFn1[L] with Serializable

  object CollectFirst extends LowPriorityCollectFirst{
    def apply[L <: HList, P <: Poly](implicit cf: CollectFirst[L, P]): Aux[L, P, cf.Out] = cf

    implicit def hlistEval[H, T <: HList, P <: Poly](implicit ev: Case1[P, H]): Aux[H :: T, P, ev.Result] =
      new CollectFirst[H :: T, P]{
        type Out = ev.Result

        def apply(l: H :: T) = ev(l.head)
      }
  }

  trait LowPriorityCollectFirst{
    type Aux[L <: HList, P <: Poly, Out0] = CollectFirst[L, P]{ type Out = Out0 }

    implicit def hlistIterate[H, T <: HList, P <: Poly, CfOut](implicit cf: CollectFirst.Aux[T, P, CfOut]): Aux[H :: T, P, CfOut] =
      new CollectFirst[H :: T, P]{
        type Out = CfOut

        def apply(l: H :: T) = cf(l.tail)
      }
  }

  private def toTuple2[Prefix, Suffix](l: Prefix :: Suffix :: HNil): (Prefix, Suffix) = (l.head, l.tail.head)


  /**
   * Typeclass witnessing that all the elements of an HList have instances of the given typeclass.
   * Courtesy of mpilquist.
   *
   * @author Tin Pavlinic
  */
  sealed trait LiftAll[F[_], In <: HList] {
    type Out <: HList
    def instances: Out
  }

  object LiftAll {

    type Aux[F[_], In0 <: HList, Out0 <: HList] = LiftAll[F, In0] { type Out = Out0 }

    class Curried[F[_]] {
      def apply[In <: HList](in: In)(implicit lift: LiftAll[F, In]): Aux[F, In, lift.Out] = lift
    }

    def apply[F[_]]: Curried[F] = new Curried[F]
    def apply[F[_], In <: HList](implicit lift: LiftAll[F, In]): Aux[F, In, lift.Out] = lift

    implicit def hnil[F[_]]: LiftAll.Aux[F, HNil, HNil] = new LiftAll[F, HNil] {
      type Out = HNil
      def instances = HNil
    }

    implicit def hcons[F[_], H, T <: HList, TI <: HList]
      (implicit headInstance: F[H], tailInstances: Aux[F, T, TI]): Aux[F, H :: T, F[H] :: TI] =
        new LiftAll[F, H :: T] {
          type Out = F[H] :: TI
          def instances = headInstance :: tailInstances.instances
    }
  }

  /**
   * Type class supporting producing a HList of shape `N` padded with elements of type `A`.
   *
   * @author ryoppy
   */
  trait PadTo[N, A, L <: HList] extends DepFn2[A, L] with Serializable { type Out <: HList }

  object PadTo {
    def apply[N, A, L <: HList](implicit padTo: PadTo[N, A, L]): Aux[N, A, L, padTo.Out] = padTo

    type Aux[N, A, L <: HList, Out0] = PadTo[N, A, L] { type Out = Out0 }

    implicit def padToHNil0[A]: Aux[_0, A, HNil, HNil] =
      new PadTo[_0, A, HNil] {
        type Out = HNil
        def apply(a: A, l: HNil) = l
      }

    implicit def padToHNil[N <: Nat, A, Out0 <: HList](implicit padTo: PadTo.Aux[N, A, HNil, Out0]): Aux[Succ[N], A, HNil, A :: Out0] =
      new PadTo[Succ[N], A, HNil] {
        type Out = A :: Out0
        def apply(a: A, l: HNil) = a :: padTo(a, l)
      }

    implicit def padTo1[N <: Nat, A, H, T <: HList, Out0 <: HList](implicit padTo: PadTo.Aux[N, A, T, Out0]): Aux[Succ[N], A, H :: T, H :: Out0] =
      new PadTo[Succ[N], A, H :: T] {
        type Out = H :: Out0
        def apply(a: A, l: H :: T) = l.head :: padTo(a, l.tail)
      }
  }

  /**
   * Type class supporting the slicing of an `HList`
   *
   * @author ryoppy
   */
  trait Slice[N, U, L <: HList] extends DepFn1[L] with Serializable { type Out <: HList }

  object Slice {
    def apply[N, U, L <: HList](implicit slice: Slice[N, U, L]): Aux[N, U, L, slice.Out] = slice

    type Aux[N, U, L <: HList, Out0] = Slice[N, U, L] { type Out = Out0 }

    implicit def slice0[L <: HList]: Aux[_0, _0, L, HNil] =
      new Slice[_0, _0, L] {
        type Out = HNil
        def apply(l: L) = HNil
      }

    implicit def slice1[N <: Nat, U <: Nat, H, T <: HList, Out0 <: HList]
      (implicit slice: Slice.Aux[N, U, T, Out0]): Aux[Succ[N], Succ[U], H :: T, Out0] =
        new Slice[Succ[N], Succ[U], H :: T] {
          type Out = Out0
          def apply(l: H :: T): Out = slice(l.tail)
        }

    implicit def slice2[U <: Nat, H, T <: HList, Out0 <: HList]
      (implicit slice: Slice.Aux[_0, U, T, Out0]): Aux[_0, Succ[U], H :: T, H :: Out0] =
        new Slice[_0, Succ[U], H :: T] {
          type Out = H :: slice.Out
          def apply(l: H :: T): Out = l.head :: slice(l.tail)
        }
  }

  /**
    * Type class supporting reifying an `HList` of singleton types.
    *
    * @author Jisoo Park
    */
  trait Reify[L <: HList] extends DepFn0 with Serializable { type Out <: HList }

  object Reify {
    def apply[L <: HList](implicit reify: Reify[L]): Aux[L, reify.Out] = reify

    type Aux[L <: HList, Out0 <: HList] = Reify[L] { type Out = Out0 }

    implicit def hnilReify[L <: HNil]: Aux[L, HNil] =
      new Reify[L] {
        type Out = HNil
        def apply(): Out = HNil
      }

    implicit def hlistReify[H, T <: HList, Out0 <: HList](implicit
      wh: Witness.Aux[H],
      rt: Reify.Aux[T, Out0]
    ) : Aux[H :: T, H :: Out0] =
      new Reify[H :: T] {
        type Out = H :: rt.Out
        def apply(): Out = wh.value :: rt()
      }
  }

  /**
   * Type class supporting the calculation of every combination of this 'HList'
   *
   * @author ryoppy
   */
  trait Combinations[N <: Nat, L <: HList] extends DepFn1[L] with Serializable {
    type Out <: HList
  }

  object Combinations extends LowPriorityCombinations {

    type Aux[N <: Nat, L <: HList, Out0 <: HList] = Combinations[N, L] { type Out = Out0 }

    def apply[N <: Nat, L <: HList](implicit c: Combinations[N, L]): Aux[N, L, c.Out] = c

    def apply[L <: HList](n: Nat, l: L)(implicit c: Combinations[n.N, L]): c.Out = c(l)

    implicit def combination0[L <: HList]: Aux[_0, L, HNil :: HNil] =
      new Combinations[_0, L] {
        type Out = HNil :: HNil
        def apply(l: L): Out = HNil :: HNil
      }

    implicit def combination1[N <: Nat, H, T <: HList, C1 <: HList, C2 <: HList, CM <: HList, CpOut <: HList]
    (implicit
     c1: Aux[N, T, C1],
     c2: Aux[Succ[N], T, C2],
     cm: MapCons.Aux[H, C1, CM],
     cp: Prepend.Aux[CM, C2, CpOut]): Aux[Succ[N], H :: T, CpOut] =
      new Combinations[Succ[N], H :: T] {
        type Out = cp.Out
        def apply(l: H :: T): Out = cp(cm(l.head, c1(l.tail)), c2(l.tail))
      }
  }

  trait LowPriorityCombinations {
    implicit def combinationHNil[N <: Nat] =
      new Combinations[N, HNil] {
        type Out = HNil
        def apply(l: HNil): Out = HNil
      }
  }

  /**
   * Type class supporting mappings from type `T` to an `HList`. Currently only supports mapping nested pairs to an `HList`
   *
   * @author Michael Zuber
   */
  sealed trait ProductToHList[-T] extends Serializable {
    type Out <: HList
    def apply(t: T): Out
  }

  object ProductToHList {
    def apply[P](implicit ev: ProductToHList[P]): ProductToHList[P] = ev

    type Aux[P, HL <: HList] = ProductToHList[P] { type Out = HL }

    implicit def pairToHCons[H, T, HL <: HList](
      implicit ev: ProductToHList.Aux[T, HL]
    ): ProductToHList.Aux[Product2[H, T], H :: HL] = new ProductToHList[Product2[H, T]] {
      type Out = H :: HL
      def apply(p: Product2[H, T]): Out = p._1 :: ev(p._2)
    }

    implicit val unitToHNil: ProductToHList.Aux[Unit, HNil] = new ProductToHList[Unit] {
      type Out = HNil
      def apply(p: Unit): Out = HNil
    }
  }

  /**
   * Type class supporting mappings from an `HList` to a nested pair
   *
   * @author Michael Zuber
   */
  sealed trait HListToProduct[HL <: HList] extends DepFn1[HL] with Serializable

  object HListToProduct {
    def apply[HL <: HList](implicit ev: HListToProduct[HL]): HListToProduct[HL] = ev

    type Aux[HL <: HList, P] = HListToProduct[HL] { type Out = P }

    implicit val hnilToUnit: HListToProduct.Aux[HNil, Unit] = new HListToProduct[HNil] {
      type Out = Unit
      def apply(hl: HNil): Out = ()
    }

    implicit def hconsToPair[H, T <: HList, TP](
      implicit ev: HListToProduct.Aux[T, TP]
    ): HListToProduct.Aux[H :: T, (H, TP)] = new HListToProduct[H :: T] {
      type Out = (H, TP)
      def apply(hl: H :: T): Out = (hl.head, ev(hl.tail))
    }
  }
}

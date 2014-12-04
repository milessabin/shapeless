/*
 * Copyright (c) 2013 Miles Sabin
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

import poly._

object coproduct {
  trait Inject[C <: Coproduct, I] {
    def apply(i: I): C
  }

  object Inject {
    def apply[C <: Coproduct, I](implicit inject: Inject[C, I]): Inject[C, I] = inject

    implicit def tlInject[H, T <: Coproduct, I](implicit tlInj : Inject[T, I]): Inject[H :+: T, I] = new Inject[H :+: T, I] {
      def apply(i: I): H :+: T = Inr(tlInj(i))
    }

    implicit def hdInject[H, T <: Coproduct]: Inject[H :+: T, H] = new Inject[H :+: T, H] {
      def apply(i: H): H :+: T = Inl(i)
    }
  }

  trait Selector[C <: Coproduct, T] {
    def apply(c: C): Option[T]
  }

  object Selector {
    def apply[C <: Coproduct, T](implicit select: Selector[C, T]): Selector[C, T] = select

    implicit def tlSelector1[H, T <: Coproduct, S](implicit st: Selector[T, S]): Selector[H :+: T, S] = new Selector[H :+: T, S] {
      def apply(c: H :+: T): Option[S] = c match {
        case Inl(h) => None
        case Inr(t) => st(t)
      }
    }

    implicit def hdSelector[H, T <: Coproduct](implicit st: Selector[T, H] = null): Selector[H :+: T, H] = new Selector[H :+: T, H] {
      def apply(c: H :+: T): Option[H] = c match {
        case Inl(h) => Some(h)
        case Inr(t) => if (st != null) st(t) else None
      }
    }
  }

  trait At[C <: Coproduct, N <: Nat] extends DepFn1[C] {
    type A
    type Out = Option[A]
  }

  object At {
    def apply[C <: Coproduct, N <: Nat](implicit at: At[C, N]): Aux[C, N, at.A] = at

    type Aux[C <: Coproduct, N <: Nat, A0] = At[C, N] { type A = A0 }

    implicit def coproductAt0[H, T <: Coproduct]: Aux[H :+: T, Nat._0, H] = new At[H :+: T, Nat._0] {
      type A = H

      def apply(c: H :+: T): Out = c match {
        case Inl(h) => Some(h)
        case _      => None
      }
    }

    implicit def coproductAtN[H, T <: Coproduct, N <: Nat](
      implicit att: At[T, N]
    ): Aux[H :+: T, Succ[N], att.A] = new At[H :+: T, Succ[N]] {
      type A = att.A

      def apply(c: H :+: T): Out = c match {
        case Inl(_)    => None
        case Inr(tail) => att(tail)
      }
    }
  }

  trait Filter[C <: Coproduct, U] extends DepFn1[C] {
    type A <: Coproduct
    type Out = Option[A]
  }

  object Filter {
    def apply[C <: Coproduct, U](implicit filter: Filter[C, U]): Aux[C, U, filter.A] = filter

    type Aux[C <: Coproduct, U, A0 <: Coproduct] = Filter[C, U] { type A = A0 }

    implicit def cnilFilter[U]: Aux[CNil, U, CNil] = new Filter[CNil, U] {
      type A = CNil

      def apply(c: CNil): Option[A] = Some(c)
    }

    implicit def coproductFilter_Match[H, T <: Coproduct, FilterT <: Coproduct](
      implicit filter: Aux[T, H, FilterT], inject: Inject[H :+: FilterT, H]
    ): Aux[H :+: T, H, H :+: FilterT] = new Filter[H :+: T, H] {
      type A = H :+: FilterT

      def apply(c: H :+: T): Option[A] = c match {
        case Inl(h) => Some(inject(h))
        case Inr(t) => filter(t).map(Inr[H, FilterT](_))
      }
    }

    implicit def coproductFilter_NonMatch[H, T <: Coproduct, FilterT <: Coproduct, U](
      implicit filter: Aux[T, U, FilterT], e: U =:!= H
    ): Aux[H :+: T, U, FilterT] = new Filter[H :+: T, U] {
      type A = FilterT

      def apply(c: H :+: T): Option[A] = c match {
        case Inr(t) => filter(t)
        case _      => None
      }
    }
  }

  trait FilterNot[C <: Coproduct, U] extends DepFn1[C] {
    type A <: Coproduct
    type Out = Option[A]
  }

  object FilterNot {
    def apply[C <: Coproduct, U](implicit filterNot: FilterNot[C, U]): Aux[C, U, filterNot.A] = filterNot

    type Aux[C <: Coproduct, U, A0 <: Coproduct] = FilterNot[C, U] { type A = A0 }

    implicit def cnilFilterNot[U]: Aux[CNil, U, CNil] = new FilterNot[CNil, U] {
      type A = CNil

      def apply(c: CNil): Option[A] = Some(c)
    }

    implicit def coproductFilterNot_Match[H, T <: Coproduct, TFilterNotH <: Coproduct](
      implicit filterNot: Aux[T, H, TFilterNotH]
    ): Aux[H :+: T, H, TFilterNotH] = new FilterNot[H :+: T, H] {
      type A = TFilterNotH

      def apply(c: H :+: T): Option[A] = c match {
        case Inr(t) => filterNot(t)
        case _      => None
      }
    }

    implicit def coproductFilterNot_NonMatch[H, T <: Coproduct, TFilterNotU <: Coproduct, U](
      implicit filterNot: Aux[T, U, TFilterNotU], inject: Inject[H :+: TFilterNotU, H], e: U =:!= H
    ): Aux[H :+: T, U, H :+: TFilterNotU] = new FilterNot[H :+: T, U] {
      type A = H :+: TFilterNotU

      def apply(c: H :+: T): Option[A] = c match {
        case Inl(h) => Some(inject(h))
        case Inr(t) => filterNot(t).map(Inr[H, TFilterNotU](_))
      }
    }
  }

  trait Remove[C <: Coproduct, U] extends DepFn1[C] {
    type Rest <: Coproduct
    type Out = Either[U, Rest]
    def inverse(r: Either[U, Rest]): C

    def coproduct(c: C): U :+: Rest = apply(c) match {
      case Left(u)  => Inl(u)
      case Right(r) => Inr(r)
    }
  }

  trait LowPriorityRemove {
    type Aux[C <: Coproduct, U, Rest0 <: Coproduct] = Remove[C, U] { type Rest = Rest0 }

    // Must be given a lower priority than removeHead, so that:
    // - the two don't collide for coproducts with repeated types
    // - the first element of type I in C is removed 
    implicit def removeTail[H, T <: Coproduct, U](implicit
      tailRemove: Remove[T, U]
    ): Aux[H :+: T, U, H :+: tailRemove.Rest] = new Remove[H :+: T, U] {
      type Rest = H :+: tailRemove.Rest

      def apply(c: H :+: T) = c match {
        case Inl(h) => Right(Inl(h))
        case Inr(t) => tailRemove(t) match {
          case Left(i)  => Left(i)
          case Right(r) => Right(Inr(r))
        }
      }

      def inverse(r: Either[U, H :+: tailRemove.Rest]) = r match {
        case Left(i)       => Inr(tailRemove.inverse(Left(i)))
        case Right(Inl(h)) => Inl(h)
        case Right(Inr(t)) => Inr(tailRemove.inverse(Right(t)))
      }
    }
  }

  object Remove extends LowPriorityRemove {
    def apply[C <: Coproduct, U](implicit remove: Remove[C, U]): Aux[C, U, remove.Rest] = remove

    implicit def removeHead[H, T <: Coproduct]: Aux[H :+: T, H, T] = new Remove[H :+: T, H] {
      type Rest = T

      def apply(c: H :+: T) = c match {
        case Inl(h) => Left(h)
        case Inr(t) => Right(t)
      }

      def inverse(r: Either[H, T]) = r match {
        case Left(h)  => Inl(h)
        case Right(t) => Inr(t)
      }
    }
  }

  trait RemoveLast[C <: Coproduct, I] extends DepFn1[C] {
    type Rest <: Coproduct
    type Out = Either[I, Rest]
    def inverse(r: Either[I, Rest]): C
  }

  trait LowPriorityRemoveLast {
    type Aux[C <: Coproduct, I, Rest0 <: Coproduct] = RemoveLast[C, I] {type Rest = Rest0}

    protected def fromRemove[C <: Coproduct, I](remove: Remove[C, I]): Aux[C, I, remove.Rest] =
      new RemoveLast[C, I] {
        type Rest = remove.Rest
        def apply(c: C) = remove(c)
        def inverse(r: Either[I, Rest]) = remove.inverse(r)
      }

    protected def toRemove[C <: Coproduct, I](removeLast: RemoveLast[C, I]): Remove.Aux[C, I, removeLast.Rest] =
      new Remove[C, I] {
        type Rest = removeLast.Rest
        def apply(c: C) = removeLast(c)
        def inverse(r: Either[I, Rest]) = removeLast.inverse(r)
      }

    // Must be given a lower priority than removeLastTail, so that:
    // - the two don't collide for coproducts with repeated types
    // - the last element of type I in C is removed
    implicit def removeLastHead[H, T <: Coproduct]: Aux[H :+: T, H, T] = fromRemove(Remove.removeHead[H, T])
  }

  object RemoveLast extends LowPriorityRemoveLast {
    def apply[C <: Coproduct, I](implicit removeLast: RemoveLast[C, I]): Aux[C, I, removeLast.Rest] = removeLast

    implicit def removeLastTail[H, T <: Coproduct, I](implicit
      tailRemoveLast: RemoveLast[T, I]
    ): Aux[H :+: T, I, H :+: tailRemoveLast.Rest] = fromRemove(Remove.removeTail(toRemove(tailRemoveLast)))
  }
  
  trait FlatMap[C <: Coproduct, F <: Poly] extends DepFn1[C] { type Out <: Coproduct }

  object FlatMap {
    def apply[C <: Coproduct, F <: Poly](implicit folder: FlatMap[C, F]): Aux[C, F, folder.Out] = folder

    type Aux[C <: Coproduct, F <: Poly, Out0 <: Coproduct] = FlatMap[C, F] { type Out = Out0 }

    implicit def cnilFlatMap[F <: Poly]: Aux[CNil, F, CNil] = new FlatMap[CNil, F] {
      type Out = CNil

      def apply(c: CNil): Out = c
    }

    implicit def cpFlatMap[H, T <: Coproduct, F <: Poly, OutH <: Coproduct, OutT <: Coproduct](
      implicit
       fh: Case1.Aux[F, H, OutH],
       ft: FlatMap.Aux[T, F, OutT],
       extendBy: ExtendBy[OutH, OutT]
    ): Aux[H :+: T, F, extendBy.Out] = new FlatMap[H :+: T, F] {
      type Out = extendBy.Out

      def apply(c: H :+: T): Out = c match {
        case Inl(h) => extendBy.right(fh(h))
        case Inr(t) => extendBy.left(ft(t))
      }
    }

  }

  trait Mapper[F <: Poly, C <: Coproduct] extends DepFn1[C] { type Out <: Coproduct }

  object Mapper {
    def apply[F <: Poly, C <: Coproduct](implicit mapper: Mapper[F, C]): Aux[F, C, mapper.Out] = mapper
    def apply[C <: Coproduct](f: Poly)(implicit mapper: Mapper[f.type, C]): Aux[f.type, C, mapper.Out] = mapper

    type Aux[F <: Poly, C <: Coproduct, Out0 <: Coproduct] = Mapper[F, C] { type Out = Out0 }

    implicit def cnilMapper[F <: Poly]: Aux[F, CNil, CNil] = new Mapper[F, CNil] {
      type Out = CNil
      def apply(t: CNil): Out = t
    }

    implicit def cpMapper[F <: Poly, H, OutH, T <: Coproduct]
      (implicit fh: Case1.Aux[F, H, OutH], mt: Mapper[F, T]): Aux[F, H :+: T, OutH :+: mt.Out] =
        new Mapper[F, H :+: T] {
          type Out = OutH :+: mt.Out
          def apply(c: H :+: T): Out = c match {
            case Inl(h) => Inl(fh(h))
            case Inr(t) => Inr(mt(t))
          }
        }
  }

  trait Unifier[C <: Coproduct] extends DepFn1[C]

  object Unifier {
    def apply[C <: Coproduct](implicit unifier: Unifier[C]): Aux[C, unifier.Out] = unifier

    type Aux[C <: Coproduct, Out0] = Unifier[C] { type Out = Out0 }

    implicit def lstUnifier[H]: Aux[H :+: CNil, H] =
      new Unifier[H :+: CNil] {
        type Out = H
        def apply(c: H :+: CNil): Out = (c: @unchecked) match {
          case Inl(h) => h
        }
      }

    implicit def cpUnifier[H1, H2, T <: Coproduct, L, Out0]
      (implicit lt: Aux[H2 :+: T, L], u: Lub[H1, L, Out0]): Aux[H1 :+: H2 :+: T, Out0] =
        new Unifier[H1 :+: H2 :+: T] {
          type Out = Out0
          def apply(c: H1 :+: H2 :+: T): Out = c match {
            case Inl(h1) => u.left(h1)
            case Inr(t) => u.right(lt(t))
          }
        }
  }

  trait Folder[F <: Poly, C <: Coproduct] extends DepFn1[C]

  object Folder {
    def apply[F <: Poly, C <: Coproduct](implicit folder: Folder[F, C]): Aux[F, C, folder.Out] = folder
    def apply[C <: Coproduct](f: Poly)(implicit folder: Folder[f.type, C]): Aux[f.type, C, folder.Out] = folder

    type Aux[F <: Poly, C <: Coproduct, Out0] = Folder[F, C] { type Out = Out0 }

    implicit def mkFolder[F <: Poly, C <: Coproduct, M <: Coproduct, Out0]
      (implicit mapper: Mapper.Aux[F, C, M], unifier: Unifier.Aux[M, Out0]): Aux[F, C, Out0] =
        new Folder[F, C] {
          type Out = Out0
          def apply(c: C): Out = unifier(mapper(c))
        }
  }

  trait ZipWithKeys[K <: HList, V <: Coproduct] extends DepFn1[V] { type Out <: Coproduct }

  object ZipWithKeys {
    import shapeless.labelled._

    def apply[K <: HList, V <: Coproduct]
      (implicit zipWithKeys: ZipWithKeys[K, V]): Aux[K, V, zipWithKeys.Out] = zipWithKeys

    type Aux[K <: HList, V <: Coproduct, Out0 <: Coproduct] = ZipWithKeys[K, V] { type Out = Out0 }

    implicit val cnilZipWithKeys: Aux[HNil, CNil, CNil] = new ZipWithKeys[HNil, CNil] {
      type Out = CNil
      def apply(v: CNil) = v
    }

    implicit def cpZipWithKeys[KH, VH, KT <: HList, VT <: Coproduct] (implicit zipWithKeys: ZipWithKeys[KT, VT], wkh: Witness.Aux[KH])
        : Aux[KH :: KT, VH :+: VT, FieldType[KH, VH] :+: zipWithKeys.Out] =
          new ZipWithKeys[KH :: KT, VH :+: VT] {
            type Out = FieldType[KH, VH] :+: zipWithKeys.Out
            def apply(v: VH :+: VT): Out = v match {
              case Inl(vh) => Inl(field[wkh.T](vh))
              case Inr(vt) => Inr(zipWithKeys(vt))
            }
          }
  }

  /**
   * Type class supporting computing the type-level Nat corresponding to the length of this `Coproduct'.
   *
   * @author Stacy Curl
   */
  trait Length[C <: Coproduct] extends DepFn0 { type Out <: Nat }

  object Length {
    def apply[C <: Coproduct](implicit length: Length[C]): Aux[C, length.Out] = length

    type Aux[C <: Coproduct, Out0 <: Nat] = Length[C] { type Out = Out0 }

    implicit def cnilLength: Aux[CNil, Nat._0] = new Length[CNil] {
      type Out = Nat._0

      def apply(): Out = Nat._0
    }

    implicit def coproductLength[H, T <: Coproduct, N <: Nat]
      (implicit lt: Aux[T, N], sn: Witness.Aux[Succ[N]]): Aux[H :+: T, Succ[N]] = new Length[H :+: T] {
        type Out = Succ[N]

        def apply(): Out = sn.value
      }

  }

  /**
   * Type class supporting extending a coproduct on the right
   *
   * @author Stacy Curl
   */
  trait ExtendRight[C <: Coproduct, T] extends DepFn1[C] { type Out <: Coproduct }

  object ExtendRight {
    def apply[C <: Coproduct, T]
      (implicit extendRight: ExtendRight[C, T]): Aux[C, T, extendRight.Out] = extendRight

    type Aux[C <: Coproduct, T, Out0 <: Coproduct] = ExtendRight[C, T] { type Out = Out0 }

    implicit def extendRightSingleton[H, A]: Aux[H :+: CNil, A, H :+: A :+: CNil] =
      new ExtendRight[H :+: CNil, A] {
        type Out = H :+: A :+: CNil

        def apply(c: H :+: CNil): Out = c match {
          case Inl(h) => Inl(h)
          case Inr(t) => Inr(Inr(t))
        }
      }

    implicit def extendRightCoproduct[H, T <: Coproduct, A, AT <: Coproduct]
      (implicit extendRight: Aux[T, A, AT]): Aux[H :+: T, A, H :+: AT] =
        new ExtendRight[H :+: T, A] {
          type Out = H :+: AT

          def apply(c: H :+: T) = c match {
            case Inl(h) => Inl(h)
            case Inr(t) => Inr(extendRight(t))
          }
        }
  }

  trait ExtendBy[L <: Coproduct, R <: Coproduct] {
    type Out <: Coproduct

    def right(l: L): Out
    def left(r: R): Out
  }

  object ExtendBy {
    def apply[L <: Coproduct, R <: Coproduct]
      (implicit extendBy: ExtendBy[L, R]): Aux[L, R, extendBy.Out] = extendBy

    type Aux[L <: Coproduct, R <: Coproduct, Out0 <: Coproduct] = ExtendBy[L, R] { type Out = Out0 }

    implicit def extendBy[L <: Coproduct, R <: Coproduct, Out0 <: Coproduct](
      implicit extendLeftBy: ExtendLeftBy.Aux[L, R, Out0], extendRightBy: ExtendRightBy.Aux[L, R, Out0]
    ): ExtendBy.Aux[L, R, Out0] = new ExtendBy[L, R] {
      type Out = Out0

      def right(l: L): Out = extendRightBy(l)
      def left(r: R): Out = extendLeftBy(r)
    }
  }

  trait ExtendLeftBy[L <: Coproduct, R <: Coproduct] extends DepFn1[R] { type Out <: Coproduct }

  object ExtendLeftBy {
    def apply[L <: Coproduct, R <: Coproduct]
      (implicit extendLeftBy: ExtendLeftBy[L, R]): Aux[L, R, extendLeftBy.Out] = extendLeftBy

    type Aux[L <: Coproduct, R <: Coproduct, Out0 <: Coproduct] = ExtendLeftBy[L, R] { type Out = Out0 }

    implicit def extendLeftByCoproduct[L <: Coproduct, R <: Coproduct, RevL <: Coproduct](
      implicit reverseL: Reverse.Aux[L, RevL], impl: Impl[RevL, R]
    ): Aux[L, R, impl.Out] = new ExtendLeftBy[L, R] {
      type Out = impl.Out

      def apply(r: R): Out = impl(r)
    }

    trait Impl[RevL <: Coproduct, R <: Coproduct] extends DepFn1[R] { type Out <: Coproduct }

    object Impl {
      type Aux[RevL <: Coproduct, R <: Coproduct, Out0 <: Coproduct] = Impl[RevL, R] { type Out = Out0 }

      implicit def extendLeftByCNilImpl[R <: Coproduct]: Aux[CNil, R, R] = new Impl[CNil, R] {
        type Out = R

        def apply(r: R): Out = r
      }

      implicit def extendLeftByCoproductImpl[H, T <: Coproduct, R <: Coproduct](
        implicit extendLeftBy: Impl[T, H :+: R]
      ): Aux[H :+: T, R, extendLeftBy.Out] = new Impl[H :+: T, R] {
        type Out = extendLeftBy.Out

        def apply(r: R): Out = extendLeftBy(Inr[H, R](r))
      }
    }
  }

  trait ExtendRightBy[L <: Coproduct, R <: Coproduct] extends DepFn1[L] { type Out <: Coproduct }

  object ExtendRightBy {
    def apply[L <: Coproduct, R <: Coproduct]
      (implicit extendRightBy: ExtendRightBy[L, R]): Aux[L, R, extendRightBy.Out] = extendRightBy

    type Aux[L <: Coproduct, R <: Coproduct, Out0 <: Coproduct] = ExtendRightBy[L, R] { type Out = Out0 }

    implicit def extendRightByCNil[L <: Coproduct]: Aux[L, CNil, L] = new ExtendRightBy[L, CNil] {
      type Out = L

      def apply(l: L): Out = l
    }

    implicit def extendRightByCoproduct[L <: Coproduct, H, LH <: Coproduct, T <: Coproduct](
      implicit extendRight: ExtendRight.Aux[L, H, LH], extendRightBy: ExtendRightBy[LH, T]
    ): Aux[L, H :+: T, extendRightBy.Out] = new ExtendRightBy[L, H :+: T] {
      type Out = extendRightBy.Out

      def apply(l: L): Out = extendRightBy(extendRight(l))
    }
  }

  /**
   * Type class supporting rotating a Coproduct left
   *
   * @author Stacy Curl
   */
  trait RotateLeft[C <: Coproduct, N <: Nat] extends DepFn1[C] { type Out <: Coproduct }

  object RotateLeft extends LowPriorityRotateLeft {
    def apply[C <: Coproduct, N <: Nat]
      (implicit rotateLeft: RotateLeft[C, N]): Aux[C, N, rotateLeft.Out] = rotateLeft

    implicit def implToRotateLeft[C <: Coproduct, N <: Nat, Size <: Nat, NModSize <: Succ[_]]
      (implicit
       length: Length.Aux[C, Size],
       mod: nat.Mod.Aux[N, Size, NModSize],
       impl: Impl[C, NModSize]
      ): Aux[C, N, impl.Out] = new RotateLeft[C, N] {
        type Out = impl.Out

        def apply(c: C): Out = impl(c)
      }

    trait Impl[C <: Coproduct, N <: Nat] extends DepFn1[C] { type Out <: Coproduct }

    object Impl {
      type Aux[C <: Coproduct, N <: Nat, Out0 <: Coproduct] = Impl[C, N] { type Out = Out0 }

      implicit def rotateCoproductOne[H, T <: Coproduct, TH <: Coproduct]
        (implicit extendRight: ExtendRight.Aux[T, H, TH], inject: Inject[TH, H]): Aux[H :+: T, Nat._1, TH] =
         new Impl[H :+: T, Nat._1] {
           type Out = TH

           def apply(c: H :+: T): Out = c match {
             case Inl(a)    => inject(a)
             case Inr(tail) => extendRight(tail)
           }
         }

      implicit def rotateCoproductN[C <: Coproduct, N <: Nat, CN <: Coproduct, CSN <: Coproduct]
        (implicit rotateN: Aux[C, N, CN], rotate1: Aux[CN, Nat._1, CSN]): Aux[C, Succ[N], CSN] =
          new Impl[C, Succ[N]] {
            type Out = CSN

            def apply(c: C): Out = rotate1(rotateN(c))
          }
    }
  }

  trait LowPriorityRotateLeft {
    type Aux[C <: Coproduct, N <: Nat, Out0 <: Coproduct] = RotateLeft[C, N] { type Out = Out0 }

    implicit def noopRotateLeftImpl[C <: Coproduct, N <: Nat]: Aux[C, N, C] = new RotateLeft[C, N] {
      type Out = C

      def apply(c: C): Out = c
    }
  }

  /**
   * Type class supporting rotating a Coproduct right
   *
   * @author Stacy Curl
   */
  trait RotateRight[C <: Coproduct, N <: Nat] extends DepFn1[C] { type Out <: Coproduct }

  object RotateRight extends LowPriorityRotateRight {
    def apply[C <: Coproduct, N <: Nat]
      (implicit rotateRight: RotateRight[C, N]): Aux[C, N, rotateRight.Out] = rotateRight

    implicit def hlistRotateRightt[
      C <: Coproduct, N <: Nat, Size <: Nat, NModSize <: Succ[_], Size_Diff_NModSize <: Nat
    ](implicit
      length: Length.Aux[C, Size],
      mod: nat.Mod.Aux[N, Size, NModSize],
      diff: nat.Diff.Aux[Size, NModSize, Size_Diff_NModSize],
      rotateLeft: RotateLeft.Impl[C, Size_Diff_NModSize]
    ): Aux[C, N, rotateLeft.Out] = new RotateRight[C, N] {
      type Out = rotateLeft.Out

      def apply(c: C): Out = rotateLeft(c)
    }
  }

  trait LowPriorityRotateRight {
    type Aux[C <: Coproduct, N <: Nat, Out0 <: Coproduct] = RotateRight[C, N] { type Out = Out0 }

    implicit def noopRotateRight[C <: Coproduct, N <: Nat]: Aux[C, N, C] = new RotateRight[C, N] {
      type Out = C

      def apply(c: C): Out = c
    }
  }

  /**
   * Type class providing access to head and tail of a Coproduct
   *
   * @author Stacy Curl
   */
  trait IsCCons[C <: Coproduct] {
    type H
    type T <: Coproduct

    def head(c: C): Option[H]
    def tail(c: C): Option[T]
  }

  object IsCCons {
    def apply[C <: Coproduct](implicit isCCons: IsCCons[C]): Aux[C, isCCons.H, isCCons.T] = isCCons

    type Aux[C <: Coproduct, H0, T0 <: Coproduct] = IsCCons[C] { type H = H0; type T = T0 }

    implicit def coproductCCons[H0, T0 <: Coproduct]: Aux[H0 :+: T0, H0, T0] = new IsCCons[H0 :+: T0] {
      type H = H0
      type T = T0

      def head(c: H0 :+: T0): Option[H0] = c match {
        case Inl(h) => Some(h)
        case _      => None
      }

      def tail(c: H0 :+: T0): Option[T0] = c match {
        case Inr(t) => Some(t)
        case _      => None
      }
    }
  }
  /**
   * Type class supporting splitting this `Coproduct` at the ''nth'' element returning prefix and suffix as a coproduct
   *
   * @author Stacy Curl, Alexandre Archambault
   */
  trait Split[C <: Coproduct, N <: Nat] extends DepFn1[C] {
    type Left  <: Coproduct
    type Right <: Coproduct
    type Out = Either[Left, Right]

    def coproduct(c: C): Left :+: Right :+: CNil = apply(c) match {
      case Left(l) =>
        Inl(l)
      case Right(r) =>
        Inr(Inl(r))
    }
  }

  object Split {
    def apply[C <: Coproduct, N <: Nat](implicit split: Split[C, N]): Aux[C, N, split.Left, split.Right] = split

    type Aux[C <: Coproduct, N <: Nat, L <: Coproduct, R <: Coproduct] =
      Split[C, N] { type Left = L; type Right = R }

    implicit def splitZero[C <: Coproduct]: Aux[C, Nat._0, CNil, C] =
      new Split[C, Nat._0] {
        type Left  = CNil
        type Right = C
        def apply(c: C) = Right(c)
      }

    implicit def splitSucc[H, T <: Coproduct, N <: Nat]
     (implicit tail: Split[T, N]): Aux[H :+: T, Succ[N], H :+: tail.Left, tail.Right] =
      new Split[H :+: T, Succ[N]] {
        type Left  = H :+: tail.Left
        type Right = tail.Right
        def apply(c: H :+: T) = c match {
          case Inl(h) => Left(Inl(h))
          case Inr(t) => tail(t) match {
            case Left(l)  => Left(Inr(l))
            case Right(r) => Right(r)
          }
        }
      }
  }

  /**
   * Type class supporting taking the first `n`-elements of this `Coproduct`
   *
   * @author Alexandre Archambault
   */
  trait Take[C <: Coproduct, N <: Nat] extends DepFn1[C] {
    type Taken <: Coproduct
    type Out = Option[Taken]
  }

  object Take {
    def apply[C <: Coproduct, N <: Nat](implicit take: Take[C, N]): Aux[C, N, take.Taken] = take

    type Aux[C <: Coproduct, N <: Nat, L <: Coproduct] = Take[C, N] { type Taken = L }
    
    implicit def takeZero[C <: Coproduct]: Aux[C, Nat._0, CNil] =
      new Take[C, Nat._0] {
        type Taken = CNil
        def apply(c: C) = None
      }
    
    implicit def takeSucc[H, T <: Coproduct, N <: Nat]
     (implicit tail: Take[T, N]): Aux[H :+: T, Succ[N], H :+: tail.Taken] =
      new Take[H :+: T, Succ[N]] {
        type Taken = H :+: tail.Taken
        def apply(c: H :+: T) = c match {
          case Inl(h) => Some(Coproduct[H :+: tail.Taken](h))
          case Inr(t) => tail(t).map(Inr[H, tail.Taken](_))
        }
      }
  }

  /**
   * Type class supporting dropping the first `n`-elements of this `Coproduct`
   *
   * @author Alexandre Archambault
   */
  trait Drop[C <: Coproduct, N <: Nat] extends DepFn1[C] {
    type Remaining <: Coproduct
    type Out = Option[Remaining]
  }

  object Drop {
    def apply[C <: Coproduct, N <: Nat](implicit drop: Drop[C, N]): Aux[C, N, drop.Remaining] = drop

    type Aux[C <: Coproduct, N <: Nat, L <: Coproduct] = Drop[C, N] { type Remaining = L }

    implicit def dropZero[C <: Coproduct]: Aux[C, Nat._0, C] =
      new Drop[C, Nat._0] {
        type Remaining = C
        def apply(c: C) = Some(c)
      }

    implicit def dropSucc[H, T <: Coproduct, N <: Nat]
     (implicit tail: Drop[T, N]): Aux[H :+: T, Succ[N], tail.Remaining] =
      new Drop[H :+: T, Succ[N]] {
        type Remaining = tail.Remaining
        def apply(c: H :+: T) = c match {
          case Inl(h) => None
          case Inr(t) => tail(t)
        }
      }
  }

  /**
   * Type class supporting reversing a Coproduct
   *
   * @author Stacy Curl
   */
  trait Reverse[C <: Coproduct] extends DepFn1[C] { type Out <: Coproduct }

  object Reverse {
    def apply[C <: Coproduct](implicit reverse: Reverse[C]): Aux[C, reverse.Out] = reverse

    type Aux[C <: Coproduct, Out0 <: Coproduct] = Reverse[C] { type Out = Out0 }

    implicit val reverseCNil: Aux[CNil, CNil] = new Reverse[CNil] {
      type Out = CNil

      def apply(c: CNil): Out = c
    }

    implicit def reverseCoproduct[
      H, T <: Coproduct, ReverseT <: Coproduct, RotateL_HReverseT <: Coproduct
    ](
      implicit
      reverse: Aux[T, ReverseT],
      rotateLeft: RotateLeft.Aux[H :+: ReverseT, Nat._1, RotateL_HReverseT],
      inject: Inject[RotateL_HReverseT, H]
    ): Aux[H :+: T, RotateL_HReverseT] = new Reverse[H :+: T] {
      type Out = RotateL_HReverseT

      def apply(c: H :+: T): Out = c match {
        case Inl(h) => inject(h)
        case Inr(t) => rotateLeft(Inr[H, ReverseT](reverse(t)))
      }
    }
  }

  /**
   * Type class supporting permuting this `Coproduct` into the same order as another `Coproduct` with
   * the same element types.
   *
   * @author Michael Pilquist
   */
  trait Align[A <: Coproduct, B <: Coproduct] extends (A => B) {
    def apply(a: A): B
  }

  object Align {
    def apply[A <: Coproduct, B <: Coproduct](implicit a: Align[A, B]): Align[A, B] = a

    implicit val cnilAlign: Align[CNil, CNil] = new Align[CNil, CNil] {
      def apply(c: CNil): CNil = c
    }

    implicit def coproductAlign[A <: Coproduct, BH, BT <: Coproduct, R <: Coproduct]
      (implicit remove: Remove.Aux[A, BH, R], alignTail: Align[R, BT]): Align[A, BH :+: BT] = new Align[A, BH :+: BT] {
      def apply(a: A) = remove(a) match {
        case Left(bh) => Inl(bh)
        case Right(rest) => Inr(alignTail(rest))
      }
    }
  }

  /**
   * Type class providing access to init and last of a Coproduct
   *
   * @author Stacy Curl
   */
  trait InitLast[C <: Coproduct] {
    type I <: Coproduct
    type L

    def init(c: C): Option[I]
    def last(c: C): Option[L]
  }

  object InitLast {
    def apply[C <: Coproduct](implicit initLast: InitLast[C]): Aux[C, initLast.I, initLast.L] = initLast

    type Aux[C <: Coproduct, I0 <: Coproduct, L0] = InitLast[C] { type I = I0; type L = L0 }

    implicit def initLastCoproduct[C <: Coproduct, ReverseC <: Coproduct, H, T <: Coproduct](
      implicit reverse: Reverse.Aux[C, ReverseC], isCCons: IsCCons.Aux[ReverseC, H, T]
    ): Aux[C, T, H] = new InitLast[C] {
      type I = T
      type L = H

      def init(c: C): Option[I] = isCCons.tail(reverse(c))
      def last(c: C): Option[L] = isCCons.head(reverse(c))
    }
  }

  implicit object cnilOrdering extends Ordering[CNil] {
    def compare(x: CNil, y: CNil) = 0
  }

  implicit def coproductPartialOrdering[H, T <: Coproduct]
    (implicit ordering: Ordering[H], partialOrdering: PartialOrdering[T]): PartialOrdering[H :+: T] =
      new PartialOrdering[H :+: T] {
        def lteq(x: H :+: T, y: H :+: T): Boolean = (x, y) match {
          case (Inl(xh), Inl(yh)) => ordering.compare(xh, yh) <= 0
          case (Inr(xt), Inr(yt)) => partialOrdering.tryCompare(xt, yt).fold(false)(_ <= 0)
          case _                  => false
        }

        def tryCompare(x: H :+: T, y: H :+: T): Option[Int] = (x, y) match {
          case (Inl(xh), Inl(yh)) => Some(ordering.compare(xh, yh))
          case (Inr(xt), Inr(yt)) => partialOrdering.tryCompare(xt, yt)
          case _                  => None
        }
      }

  /**
   * Type class computing the `HList`  type corresponding to this `Coproduct`.
   *
   * @author Miles Sabin
   */
  trait ToHList[L <: Coproduct] { type Out <: HList }

  object ToHList {
    def apply[L <: Coproduct](implicit thl: ToHList[L]): Aux[L, thl.Out] = thl

    type Aux[L <: Coproduct, Out0 <: HList] = ToHList[L] { type Out = Out0 }

    implicit val cnilToHList: Aux[CNil, HNil] =
      new ToHList[CNil] {
        type Out = HNil
      }

    implicit def cconsToHList[H, T <: Coproduct](implicit ut: ToHList[T]): Aux[H :+: T, H :: ut.Out] =
      new ToHList[H :+: T] {
        type Out = H :: ut.Out
      }
  }


  /**
    * Typeclass checking that :
    * - coproduct is a sub-union of a bigger coproduct
    * - embeds a sub-coproduct into a bigger coproduct
    */
  trait Basis[Super <: Coproduct, Sub <: Coproduct] extends DepFn1[Super] {
    type Rest <: Coproduct
    type Out = Either[Rest, Sub]
    def inverse(e: Either[Rest, Sub]): Super
  }

  object Basis {
    type Aux[Super <: Coproduct, Sub <: Coproduct, Rest0 <: Coproduct] =
      Basis[Super, Sub] { type Rest = Rest0 }

    def apply[Super <: Coproduct, Sub <: Coproduct](implicit basis: Basis[Super, Sub]): Aux[Super, Sub, basis.Rest] = 
      basis

    implicit def cnilBasis[Super <: Coproduct]: Aux[Super, CNil, Super] = new Basis[Super, CNil] {
      type Rest = Super
      def apply(s: Super) = Left(s)
      def inverse(e: Either[Rest, CNil]) = e.left.get // No CNil exists, so e cannot be a Right
    }

    implicit def cconsBasis[Super <: Coproduct, H, T <: Coproduct, TRest <: Coproduct](implicit
      tailBasis: Basis.Aux[Super, T, TRest],
      remove: RemoveLast[TRest, H]
    ): Aux[Super, H :+: T, remove.Rest] = new Basis[Super, H :+: T] {
      type Rest = remove.Rest

      def apply(s: Super) = tailBasis(s) match {
        case Left(r)  => remove(r) match {
          case Left(h)  => Right(Inl(h))
          case Right(r) => Left(r)
        }
        case Right(t) => Right(Inr(t))
      }

      def inverse(e: Either[Rest, H :+: T]) = e match {
        case Left(r)  => tailBasis.inverse(Left(remove.inverse(Right(r))))
        case Right(c) => c match {
          case Inl(h)  => tailBasis.inverse(Left(remove.inverse(Left(h))))
          case Inr(t)  => tailBasis.inverse(Right(t))
        }
      }
    }
  }
}

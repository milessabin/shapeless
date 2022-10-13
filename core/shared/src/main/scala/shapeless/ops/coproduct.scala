/*
 * Copyright (c) 2013-15 Miles Sabin
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

import annotation.implicitNotFound

object coproduct {
  trait Inject[C <: Coproduct, I] extends Serializable {
    def apply(i: I): C
  }

  object Inject {
    def apply[C <: Coproduct, I](implicit inject: Inject[C, I]): Inject[C, I] = inject

    implicit def tlInject[H, T <: Coproduct, I](implicit tlInj : Inject[T, I]): Inject[H :+: T, I] = new Inject[H :+: T, I] {
      def apply(i: I): H :+: T = Inr(tlInj(i))
    }

    implicit def hdInject[H, HH <: H, T <: Coproduct]: Inject[H :+: T, HH] = new Inject[H :+: T, HH] {
      def apply(i: HH): H :+: T = Inl(i)
    }
  }

  trait Selector[C <: Coproduct, T] extends Serializable {
    def apply(c: C): Option[T]
  }

  object Selector {
    def apply[C <: Coproduct, T](implicit select: Selector[C, T]): Selector[C, T] = select

    implicit def hdSelector[H, T <: Coproduct]: Selector[H :+: T, H] = new Selector[H :+: T, H] {
      def apply(c: H :+: T): Option[H] = c match {
        case Inl(h) => Some(h)
        case Inr(t) => None
      }
    }
    implicit def tlSelector[H, T <: Coproduct, S](implicit st: Selector[T, S]): Selector[H :+: T, S] = new Selector[H :+: T, S] {
      def apply(c: H :+: T): Option[S] = c match {
        case Inl(h) => None
        case Inr(t) => st(t)
      }
    }
  }

  trait At[C <: Coproduct, N <: Nat] extends DepFn1[C] with Serializable {
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

  trait Partition[C <: Coproduct, U] extends DepFn1[C] with Serializable {
    type Prefix <: Coproduct
    type Suffix <: Coproduct
    type Out = Either[Prefix, Suffix]

    def filter(c: C): Option[Prefix]    = apply(c).swap.toOption
    def filterNot(c: C): Option[Suffix] = apply(c).toOption
    def apply(c: C): Out = toEither(coproduct(c))
    def coproduct(c: C): Prefix :+: Suffix :+: CNil
  }

  object Partition {
    def apply[C <: Coproduct, U]
      (implicit partition: Partition[C, U]): Aux[C, U, partition.Prefix, partition.Suffix] = partition

    type Aux[C <: Coproduct, U, Prefix0 <: Coproduct, Suffix0 <: Coproduct] = Partition[C, U] {
      type Prefix = Prefix0
      type Suffix = Suffix0
    }

    implicit def cnilPartition[U]: Aux[CNil, U, CNil, CNil] = new Partition[CNil, U] {
      type Prefix = CNil
      type Suffix = CNil

      def coproduct(c: CNil): Prefix :+: Suffix :+: CNil = Inr(Inr(c))
    }

    implicit def coproductPartition_Match[H, T <: Coproduct, TPrefix <: Coproduct, TSuffix <: Coproduct](
      implicit partition: Aux[T, H, TPrefix, TSuffix]
    ): Aux[H :+: T, H, H :+: TPrefix, TSuffix] = new Partition[H :+: T, H] {
      type Prefix = H :+: TPrefix
      type Suffix = TSuffix

      def coproduct(c: H :+: T): Prefix :+: Suffix :+: CNil = c match {
        case Inl(h) => Inl(Inl(h))
        case Inr(t) => partition.coproduct(t) match {
          case Inl(h) => Inl(Inr(h))
          case Inr(t) => Inr(t)
        }
      }
    }

    implicit def coproductPartition_NonMatch[H, T <: Coproduct, TPrefix <: Coproduct, TSuffix <: Coproduct, U](
      implicit partition: Aux[T, U, TPrefix, TSuffix], e: U =:!= H
    ): Aux[H :+: T, U, TPrefix, H :+: TSuffix] = new Partition[H :+: T, U] {
      type Prefix = TPrefix
      type Suffix = H :+: TSuffix

      def coproduct(c: H :+: T): Prefix :+: Suffix :+: CNil = c match {
        case Inl(h) => Inr(Inl(Inl(h)))
        case Inr(t) => partition.coproduct(t) match {
          case Inl(h)      => Inl(h)
          case Inr(Inl(t)) => Inr(Inl(Inr(t)))
          case Inr(Inr(c)) => Inr(Inr(c))
        }
      }
    }
  }

  trait Filter[C <: Coproduct, U] extends DepFn1[C] with Serializable {
    type A <: Coproduct
    type Out = Option[A]
  }

  object Filter {
    def apply[C <: Coproduct, U](implicit filter: Filter[C, U]): Aux[C, U, filter.A] = filter

    type Aux[C <: Coproduct, U, A0 <: Coproduct] = Filter[C, U] { type A = A0 }

    implicit def coproductFilter[C <: Coproduct, U, CPrefix <: Coproduct, CSuffix <: Coproduct](
      implicit partition: Partition.Aux[C, U, CPrefix, CSuffix]
    ): Aux[C, U, CPrefix] = new Filter[C, U] {
      type A = CPrefix

      def apply(c: C): Out = partition.filter(c)
    }
  }

  trait FilterNot[C <: Coproduct, U] extends DepFn1[C] with Serializable {
    type A <: Coproduct
    type Out = Option[A]
  }

  object FilterNot {
    def apply[C <: Coproduct, U](implicit filterNot: FilterNot[C, U]): Aux[C, U, filterNot.A] = filterNot

    type Aux[C <: Coproduct, U, A0 <: Coproduct] = FilterNot[C, U] { type A = A0 }

    implicit def coproductFilterNot[C <: Coproduct, U, CPrefix <: Coproduct, CSuffix <: Coproduct](
      implicit partition: Partition.Aux[C, U, CPrefix, CSuffix]
    ): Aux[C, U, CSuffix] = new FilterNot[C, U] {
      type A = CSuffix

      def apply(c: C): Out = partition.filterNot(c)
    }
  }

  trait Remove[C <: Coproduct, U] extends DepFn1[C] with Serializable {
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

  trait RemoveLast[C <: Coproduct, I] extends DepFn1[C] with Serializable {
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

  trait FlatMap[C <: Coproduct, F <: Poly] extends DepFn1[C] with Serializable { type Out <: Coproduct }

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

  trait Mapper[F <: Poly, C <: Coproduct] extends DepFn1[C] with Serializable { type Out <: Coproduct }

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

  trait Unifier[C <: Coproduct] extends DepFn1[C] with Serializable

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

  trait Folder[F <: Poly, C <: Coproduct] extends DepFn1[C] with Serializable

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

  trait LeftFolder[C <: Coproduct, In, F] extends DepFn2[C,In] with Serializable

  object LeftFolder {
    def apply[C <: Coproduct, In, F](implicit folder: LeftFolder[C, In, F]): Aux[C, In, F, folder.Out] = folder

    type Aux[C <: Coproduct, In, HF, Out0] = LeftFolder[C, In, HF] { type Out = Out0 }

    implicit def hdLeftFolder[H, In, F]
    (implicit f: Case2.Aux[F, In, H, In]): Aux[H :+: CNil, In, F,In] = new LeftFolder[H :+: CNil, In, F] {
      type Out = In
      def apply(c: H :+: CNil, in: In): In = f(in,c.head.get)
    }

    implicit def tlLeftFolder[H, T <: Coproduct, In, HF, OutH]
    (implicit f: Case2.Aux[HF, In, H, OutH], ft: Aux[T, In, HF, OutH]): Aux[H :+: T, In, HF, OutH] = new LeftFolder[H :+: T, In, HF] {
      type Out = OutH
      def apply(c: H :+: T, in: In): Out =
        c match {
          case Inl(h) => f(in, h)
          case Inr(t) => ft(t, in)
        }
    }
  }

  /**
   * Type class supporting zipping this `Coproduct` with a constant of type `Z` returning a `Coproduct` of tuples of the form
   * ({element from input `Coproduct`}, {supplied constant})
   *
   * @author William Harvey
   */
  trait ZipConst[Z, V <: Coproduct] extends DepFn2[Z, V] with Serializable { type Out <: Coproduct }

  object ZipConst {
    def apply[Z, V <: Coproduct](implicit zipConst: ZipConst[Z, V]): Aux[Z, V, zipConst.Out] = zipConst

    type Aux[Z, V <: Coproduct, Out0 <: Coproduct] = ZipConst[Z, V] { type Out = Out0 }

    implicit def cnilZipConst[Z]: Aux[Z, CNil, CNil] = new ZipConst[Z, CNil] {
      type Out = CNil
      def apply(z: Z, v: CNil) = v
    }

    implicit def cpZipConst[Z, VH, VT <: Coproduct](implicit zipConst: ZipConst[Z, VT]): Aux[Z, VH :+: VT, (VH, Z) :+: zipConst.Out] =
      new ZipConst[Z, VH :+: VT] {
        type Out = (VH, Z) :+: zipConst.Out
        def apply(z: Z, v: VH :+: VT): Out = v match {
          case Inl(vh) => Inl((vh, z))
          case Inr(vt) => Inr(zipConst(z, vt))
        }
      }
  }

  trait ZipWithKeys[K <: HList, V <: Coproduct] extends DepFn1[V] with Serializable { type Out <: Coproduct }

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
   * Type class supporting zipping a `Coproduct` with its element indices, resulting in a `Coproduct` of tuples of the form
   * ({element from input tuple}, {element index})
   *
   * @author Andreas Koestler
   */
  trait ZipWithIndex[C <: Coproduct] extends DepFn1[C] with Serializable { type Out <: Coproduct; type Idx <: Nat }

  object ZipWithIndex {

    import shapeless.Nat._

    def apply[C <: Coproduct](implicit zipper: ZipWithIndex[C]): Aux[C, zipper.Out] = zipper

    type Aux[C <: Coproduct, Out0 <: Coproduct] = ZipWithIndex[C] {type Out = Out0}

    implicit def cpZipWithIndex[C <: Coproduct]
    (implicit impl: Impl[C, _0]): Aux[C, impl.Out] = new ZipWithIndex[C] {
      type Out = impl.Out

      def apply(c: C): Out = impl(c)
    }

    trait Impl[C <: Coproduct, N <: Nat] extends DepFn1[C] with Serializable {
      type Out <: Coproduct
    }

    object Impl {
      def apply[C <: Coproduct, N <: Nat](implicit impl: Impl[C, N]): Aux[C, N, impl.Out] = impl

      type Aux[C <: Coproduct, N <: Nat, Out0 <: Coproduct] = Impl[C, N] {type Out = Out0}

      implicit def singleZipWithIndexImpl[CH, N <: Nat]
      (implicit w: Witness.Aux[N]): Aux[CH :+: CNil, N, (CH, N) :+: CNil] = new Impl[CH :+: CNil, N] {
        type Out = (CH, N) :+: CNil

        def apply(c: CH :+: CNil): Out = Coproduct[Out]((c.head.get, w.value))
      }

      implicit def cpZipWithIndexImpl[CH, CT <: Coproduct, N <: Nat, OutC <: Coproduct]
      (implicit
       impl: Impl[CT, Succ[N]],
       w: Witness.Aux[N]
        ): Aux[CH :+: CT, N, (CH, N) :+: impl.Out] =
        new Impl[CH :+: CT, N] {
          type Out = (CH, N) :+: impl.Out

          def apply(c: CH :+: CT): Out = c match {
            case Inl(h) => Inl((h, w.value))
            case Inr(t) => Inr(impl(t))
          }
        }
    }

  }

  /**
   * Type class supporting zipping a `Coproduct` with an `HList`, resulting in a `Coproduct` of tuples of the form
   * ({element from input `Coproduct`}, {element from input `HList`})
   *
   * @author William Harvey
   */
  trait ZipWith[H <: HList, V <: Coproduct] extends DepFn2[H, V] with Serializable { type Out <: Coproduct }

  object ZipWith {
    def apply[H <: HList, V <: Coproduct](implicit zipWith: ZipWith[H, V]): Aux[H, V, zipWith.Out] = zipWith

    type Aux[H <: HList, V <: Coproduct, Out0 <: Coproduct] = ZipWith[H, V] { type Out = Out0 }

    implicit def cnilZipWith: Aux[HNil, CNil, CNil] = new ZipWith[HNil, CNil] {
      type Out = CNil
      def apply(h: HNil, v: CNil) = v
    }

    implicit def cpZipWith[HH, HT <: HList, VH, VT <: Coproduct](implicit zipWith: ZipWith[HT, VT]):
        Aux[HH :: HT, VH :+: VT, (VH, HH) :+: zipWith.Out] = new ZipWith[HH :: HT, VH :+: VT] {
      type Out = (VH, HH) :+: zipWith.Out
      def apply(h: HH :: HT, v: VH :+: VT): Out = v match {
        case Inl(vh) => Inl((vh, h.head))
        case Inr(vt) => Inr(zipWith(h.tail, vt))
      }
    }
  }

  /**
   * Type class supporting computing the type-level Nat corresponding to the length of this `Coproduct'.
   *
   * @author Stacy Curl
   */
  trait Length[C <: Coproduct] extends DepFn0 with Serializable { type Out <: Nat }

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
  trait ExtendRight[C <: Coproduct, T] extends DepFn1[C] with Serializable { type Out <: Coproduct }

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

  trait ExtendBy[L <: Coproduct, R <: Coproduct] extends Serializable {
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

  trait ExtendLeftBy[L <: Coproduct, R <: Coproduct] extends DepFn1[R] with Serializable { type Out <: Coproduct }

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

    trait Impl[RevL <: Coproduct, R <: Coproduct] extends DepFn1[R] with Serializable { type Out <: Coproduct }

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

  trait ExtendRightBy[L <: Coproduct, R <: Coproduct] extends DepFn1[L] with Serializable { type Out <: Coproduct }

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
   * @author Alexandre Archambault
   */
  trait RotateLeft[C <: Coproduct, N <: Nat] extends DepFn1[C] with Serializable { type Out <: Coproduct }

  object RotateLeft extends LowPriorityRotateLeft {
    type Aux[C <: Coproduct, N <: Nat, Out0] = RotateLeft[C, N] { type Out = Out0 }

    def apply[C <: Coproduct, N <: Nat]
     (implicit rotateLeft: RotateLeft[C, N]): Aux[C, N, rotateLeft.Out] = rotateLeft

    implicit def cnilRotateLeft[N <: Nat]: RotateLeft.Aux[CNil, N, CNil] = new RotateLeft[CNil, N] {
      type Out = CNil
      def apply(c: CNil) = c
    }

    /** Binary compatibility stub */
    def implToRotateLeft[C <: Coproduct, N <: Nat, Size <: Nat, NModSize <: Succ[_]](implicit
      length: Length.Aux[C, Size],
      mod: nat.Mod.Aux[N, Size, NModSize],
      impl: Impl[C, NModSize]
    ): Aux[C, N, impl.Out] =
      new RotateLeft[C, N] {
        type Out = impl.Out
        def apply(c: C): Out = impl(c)
      }

    /** Binary compatibility stub */
    trait Impl[C <: Coproduct, N <: Nat] extends DepFn1[C] with Serializable { type Out <: Coproduct }

    /** Binary compatibility stub */
    object Impl {
      type Aux[C <: Coproduct, N <: Nat, Out0 <: Coproduct] = Impl[C, N] { type Out = Out0 }

      def rotateCoproductOne[H, T <: Coproduct, TH <: Coproduct]
        (implicit extendRight: ExtendRight.Aux[T, H, TH], inject: Inject[TH, H]): Aux[H :+: T, Nat._1, TH] =
          new Impl[H :+: T, Nat._1] {
            type Out = TH

            def apply(c: H :+: T): Out = c match {
              case Inl(a)    => inject(a)
              case Inr(tail) => extendRight(tail)
            }
          }

      def rotateCoproductN[C <: Coproduct, N <: Nat, CN <: Coproduct, CSN <: Coproduct]
        (implicit rotateN: Aux[C, N, CN], rotate1: Aux[CN, Nat._1, CSN]): Aux[C, Succ[N], CSN] =
          new Impl[C, Succ[N]] {
            type Out = CSN

            def apply(c: C): Out = rotate1(rotateN(c))
          }
    }
  }

  trait LowPriorityRotateLeft {
    implicit def coproductRotateLeft[
     C <: Coproduct, N <: Nat, Size <: Nat, NModSize <: Nat, Before <: Coproduct, After <: Coproduct
    ](implicit
      length: Length.Aux[C, Size],
      mod: nat.Mod.Aux[N, Size, NModSize],
      split: Split.Aux[C, NModSize, Before, After],
      prepend: Prepend[After, Before]
    ): RotateLeft.Aux[C, N, prepend.Out] = new RotateLeft[C, N] {
      type Out = prepend.Out

      def apply(c: C): Out = {
        val e = split(c)

        prepend(e.swap)
      }
    }

    /** Binary compatibility stub */
    def noopRotateLeftImpl[C <: Coproduct, N <: Nat]: RotateLeft.Aux[C, N, C] = new RotateLeft[C, N] {
      type Out = C
      def apply(c: C): Out = c
    }
  }

  /**
   * Type class supporting rotating a Coproduct right
   *
   * @author Stacy Curl
   * @author Alexandre Archambault
   */
  trait RotateRight[C <: Coproduct, N <: Nat] extends DepFn1[C] with Serializable { type Out <: Coproduct }

  object RotateRight extends LowPriorityRotateRight {
    def apply[C <: Coproduct, N <: Nat]
     (implicit rotateRight: RotateRight[C, N]): Aux[C, N, rotateRight.Out] = rotateRight

    implicit def cnilRotateRight[N <: Nat]: RotateRight.Aux[CNil, N, CNil] = new RotateRight[CNil, N] {
      type Out = CNil
      def apply(c: CNil) = c
    }

    /** Binary compatibility stub */
    def hlistRotateRight[
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

    implicit def coproductRotateRight[
     C <: Coproduct, N <: Nat, Size <: Nat, NModSize <: Nat, Size_Diff_NModSize <: Nat
    ](implicit
      length: Length.Aux[C, Size],
      mod: nat.Mod.Aux[N, Size, NModSize],
      diff: nat.Diff.Aux[Size, NModSize, Size_Diff_NModSize],
      rotateLeft: RotateLeft[C, Size_Diff_NModSize]
    ): RotateRight.Aux[C, N, rotateLeft.Out] = new RotateRight[C, N] {
      type Out = rotateLeft.Out

      def apply(c: C): Out = rotateLeft(c)
    }

    /** Binary compatibility stub */
    def noopRotateRight[C <: Coproduct, N <: Nat]: Aux[C, N, C] = new RotateRight[C, N] {
      type Out = C
      def apply(c: C): Out = c
    }
  }

  /**
   * Type class providing access to head and tail of a Coproduct
   *
   * @author Stacy Curl
   */
  trait IsCCons[C <: Coproduct] extends Serializable {
    type H
    type T <: Coproduct

    def head(c: C): Option[H]
    def tail(c: C): Option[T]
    def cons(e: Either[H, T]): C
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

      def cons(e: Either[H0, T0]): H0 :+: T0 = e match {
        case Left(h) => Inl(h)
        case Right(t) => Inr(t)
      }
    }
  }
  /**
   * Type class supporting splitting this `Coproduct` at the ''nth'' element returning prefix and suffix as a coproduct
   *
   * @author Stacy Curl, Alexandre Archambault
   */
  trait Split[C <: Coproduct, N <: Nat] extends DepFn1[C] with Serializable {
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
  trait Take[C <: Coproduct, N <: Nat] extends DepFn1[C] with Serializable {
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
  trait Drop[C <: Coproduct, N <: Nat] extends DepFn1[C] with Serializable {
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
   * @author Alexandre Archambault
   */
  trait Reverse[C <: Coproduct] extends DepFn1[C] with Serializable { type Out <: Coproduct }

  object Reverse {
    def apply[C <: Coproduct](implicit reverse: Reverse[C]): Aux[C, reverse.Out] = reverse

    type Aux[C <: Coproduct, Out0 <: Coproduct] = Reverse[C] { type Out = Out0 }

    implicit def reverse[C <: Coproduct, Out0 <: Coproduct](implicit reverse: Reverse0[CNil, C, Out0]): Aux[C, Out0] =
      new Reverse[C] {
        type Out = Out0
        def apply(c: C) = reverse(Right(c))
      }

    trait Reverse0[Acc <: Coproduct, L <: Coproduct, Out <: Coproduct] extends Serializable {
      def apply(e: Either[Acc, L]): Out
    }

    object Reverse0 {
      implicit def cnilReverse[Out <: Coproduct]: Reverse0[Out, CNil, Out] =
        new Reverse0[Out, CNil, Out] {
          def apply(e: Either[Out, CNil]) = (e: @unchecked) match {
            case Left(l) => l
          }
        }

      implicit def cconsReverse[Acc <: Coproduct, InH, InT <: Coproduct, Out <: Coproduct]
       (implicit rt: Reverse0[InH :+: Acc, InT, Out]): Reverse0[Acc, InH :+: InT, Out] =
        new Reverse0[Acc, InH :+: InT, Out] {
          def apply(e: Either[Acc, InH :+: InT]) = rt(e match {
            case Left(acc) => Left(Inr(acc))
            case Right(Inl(h)) => Left(Inl(h))
            case Right(Inr(t)) => Right(t)
          })
        }
    }

    /** Binary compatibility stub */
    val reverseCNil: Aux[CNil, CNil] = reverse[CNil, CNil]

    /** Binary compatibility stub */
    def reverseCoproduct[H, T <: Coproduct, ReverseT <: Coproduct, RotateL_HReverseT <: Coproduct](implicit
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
  trait Align[A <: Coproduct, B <: Coproduct] extends (A => B) with Serializable {
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
   * Type class supporting prepending to this `Coproduct`.
   *
   * @author Alexandre Archambault
   */
  trait Prepend[P <: Coproduct, S <: Coproduct] extends DepFn1[Either[P, S]] with Serializable { type Out <: Coproduct }

  trait LowestPriorityPrepend {
    type Aux[P <: Coproduct, S <: Coproduct, Out0 <: Coproduct] = Prepend[P, S] { type Out = Out0 }

    implicit def cconsPrepend[PH, PT <: Coproduct, S <: Coproduct]
     (implicit pt : Prepend[PT, S]): Aux[PH :+: PT, S, PH :+: pt.Out] =
      new Prepend[PH :+: PT, S] {
        type Out = PH :+: pt.Out
        def apply(e : Either[PH :+: PT, S]): Out = e match {
          case Left(Inl(h)) => Inl(h)
          case Left(Inr(t)) => Inr(pt(Left(t)))
          case Right(s) => Inr(pt(Right(s)))
        }
      }
  }

  trait LowPriorityPrepend extends LowestPriorityPrepend {
    implicit def cnilPrepend0[P <: Coproduct]: Aux[P, CNil, P] =
      new Prepend[P, CNil] {
        type Out = P
        def apply(e : Either[P, CNil]): P = (e: @unchecked) match {
          case Left(l) => l
        }
      }
  }

  object Prepend extends LowPriorityPrepend {
    def apply[P <: Coproduct, S <: Coproduct](implicit prepend: Prepend[P, S]): Aux[P, S, prepend.Out] = prepend

    implicit def cnilPrepend1[S <: Coproduct]: Aux[CNil, S, S] =
      new Prepend[CNil, S] {
        type Out = S
        def apply(e: Either[CNil, S]): S = (e: @unchecked) match {
          case Right(r) => r
        }
      }
  }

  /**
   * Type class providing access to init and last of a Coproduct
   *
   * @author Stacy Curl
   */
  trait InitLast[C <: Coproduct] extends Serializable {
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
  trait ToHList[L <: Coproduct] extends Serializable { type Out <: HList }

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
  trait Basis[Super <: Coproduct, Sub <: Coproduct] extends DepFn1[Super] with Serializable {
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
      def inverse(e: Either[Rest, CNil]) = (e: @unchecked) match { // No CNil exists, so e cannot be a Right
        case Left(l) => l
      }
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

  private def toEither[Prefix, Suffix](c: Prefix :+: Suffix :+: CNil): Either[Prefix, Suffix] = c match {
    case Inl(prefix)      => Left(prefix)
    case Inr(Inl(suffix)) => Right(suffix)
    case _                => sys.error("Impossible")
  }

  /**
    * Type class supporting reifying a `Coproduct` of singleton types.
    *
    * @author Jisoo Park
    */
  trait Reify[L <: Coproduct] extends DepFn0 with Serializable { type Out <: HList }

  object Reify {
    def apply[L <: Coproduct](implicit reify: Reify[L]): Aux[L, reify.Out] = reify

    type Aux[L <: Coproduct, Out0 <: HList] = Reify[L] { type Out = Out0 }

    implicit def cnilReify[L <: CNil]: Aux[L, HNil] =
      new Reify[L] {
        type Out = HNil
        def apply(): Out = HNil
      }

    implicit def coproductReify[H, T <: Coproduct](implicit
      wh: Witness.Aux[H],
      rt: Reify[T]
    ): Aux[H :+: T, H :: rt.Out] =
      new Reify[H :+: T] {
        type Out = H :: rt.Out
        def apply(): Out = wh.value :: rt()
      }
  }

  sealed trait LiftAll[F[_], In <: Coproduct] {
    type Out <: HList
    def instances: Out
  }

  object LiftAll {
    type Aux[F[_], In0 <: Coproduct, Out0 <: HList] = LiftAll[F, In0] {type Out = Out0}

    class Curried[F[_]] {
      def apply[In <: Coproduct](in: In)(implicit ev: LiftAll[F, In]): Aux[F, In, ev.Out] = ev
    }

    def apply[F[_]]: Curried[F] = new Curried[F]
    def apply[F[_], In <: Coproduct](implicit ev: LiftAll[F, In]): Aux[F, In, ev.Out] = ev

    implicit def liftAllCnil[F[_]]: LiftAll.Aux[F, CNil, HNil] = new LiftAll[F, CNil] {
      type Out = HNil
      def instances = HNil
    }

    implicit def liftAllCcons[F[_], H, T <: Coproduct, TI <: HList]
    (implicit headInstance: F[H], tailInstances: Aux[F, T, TI]): Aux[F, H :+: T, F[H] :: TI] =
      new LiftAll[F, H :+: T] {
        type Out = F[H] :: TI
        def instances = headInstance :: tailInstances.instances
      }
  }

  /**
    * Typeclass converting a `Coproduct` to an `Either`
    *
    * @author Michael Zuber
    */
  sealed trait CoproductToEither[C <: Coproduct] extends DepFn1[C] with Serializable

  object CoproductToEither {
    type Aux[In <: Coproduct, Out0] = CoproductToEither[In] { type Out = Out0 }

    implicit def baseToEither[L, R]: CoproductToEither.Aux[L :+: R :+: CNil, Either[L, R]] = new CoproductToEither[L :+: R :+: CNil] {
      type Out = Either[L, R]
      def apply(t: L :+: R :+: CNil): Either[L, R] = t match {
        case Inl(l) => Left(l)
        case Inr(Inl(r)) => Right(r)
        case _ => ???
      }
    }

    implicit def cconsToEither[L, R <: Coproduct, Out0](implicit
      evR: CoproductToEither.Aux[R, Out0]
    ): CoproductToEither.Aux[L :+: R, Either[L, Out0]] = new CoproductToEither[L :+: R] {
      type Out = Either[L, Out0]
      def apply(t: L :+: R): Either[L, Out0] = t match {
        case Inl(l) => Left(l)
        case Inr(r) => Right(evR(r))
      }
    }
  }

  /**
    * Typeclass converting an `Either` to a `Coproduct`
    *
    * @author Michael Zuber
    */
  sealed trait EitherToCoproduct[L, R] extends DepFn1[Either[L, R]] with Serializable { type Out <: Coproduct }

  object EitherToCoproduct extends EitherToCoproductLowPrio {
    type Aux[L, R, Out0 <: Coproduct] = EitherToCoproduct[L, R] { type Out = Out0 }

    implicit def econsEitherToCoproduct[L, RL, RR, Out0 <: Coproduct](implicit
      evR: EitherToCoproduct.Aux[RL, RR, Out0]
    ): EitherToCoproduct.Aux[L, Either[RL, RR], L :+: Out0] = new EitherToCoproduct[L, Either[RL, RR]] {
      type Out = L :+: Out0
      def apply(t: Either[L, Either[RL, RR]]): L :+: Out0 = t match {
        case Left(l) => Inl(l)
        case Right(r) => Inr(evR(r))
      }
    }
  }

  trait EitherToCoproductLowPrio {
    implicit def baseEitherToCoproduct[L, R]: EitherToCoproduct.Aux[L, R, L :+: R :+: CNil] = new EitherToCoproduct[L, R] {
      type Out = L :+: R :+: CNil

      def apply(t: Either[L, R]): L :+: R :+: CNil = t match {
        case Left(l) => Inl(l)
        case Right(r) => Coproduct[L :+: R :+: CNil](r)
      }
    }
  }

  /**
    * Type class supporting the injection of runtime values of type `Any` in `Coproduct`.
    *
    * @author Juan José Vázquez Delgado
    * @author Fabio Labella
    */
  @implicitNotFound("Implicit not found. CNil has no values, so it's impossible to convert anything to it")
  trait RuntimeInject[C <: Coproduct] extends Serializable {
    def apply(x: Any): Option[C]
  }

  object RuntimeInject extends RuntimeInjectLowPrio {
    implicit def baseCaseRuntimeInject[H](
        implicit castH: Typeable[H]): RuntimeInject[H :+: CNil] =
      new RuntimeInject[H :+: CNil] {
        def apply(x: Any): Option[H :+: CNil] =
          castH.cast(x).map(v => Inl(v))
      }
  }

  trait RuntimeInjectLowPrio {
    implicit def inductiveCaseRuntimeInject[H, T <: Coproduct](
        implicit next: RuntimeInject[T],
        castH: Typeable[H]): RuntimeInject[H :+: T] =
      new RuntimeInject[H :+: T] {
        def apply(x: Any): Option[H :+: T] = castH.cast(x) match {
          case Some(value) => Option(Inl(value))
          case None => next(x).map(v => Inr(v))
        }
      }
  }
}

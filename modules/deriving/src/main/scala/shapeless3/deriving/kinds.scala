/*
 * Copyright (c) 2019 Miles Sabin
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

package shapeless3.deriving

import scala.compiletime._
import scala.deriving._

object K0 {
  type Kind[C, O] = C { type Kind = K0.type ; type MirroredType = O ; type MirroredElemTypes }
  type Generic[O] = Kind[Mirror, O]
  type ProductGeneric[O] = Kind[Mirror.Product, O]
  type CoproductGeneric[O] = Kind[Mirror.Sum, O]

  def Generic[O](using gen: Generic[O]): gen.type = gen
  def ProductGeneric[O](using gen: ProductGeneric[O]): gen.type = gen
  def CoproductGeneric[O](using gen: CoproductGeneric[O]): gen.type = gen

  type Instances[F[_], T] = ErasedInstances[K0.type, F[T]]
  type ProductInstances[F[_], T] = ErasedProductInstances[K0.type, F[T]]
  type CoproductInstances[F[_], T] = ErasedCoproductInstances[K0.type, F[T]]

  def Instances[F[_], T](using inst: Instances[F, T]): inst.type = inst
  def ProductInstances[F[_], T](using inst: ProductInstances[F, T]): inst.type = inst
  def CoproductInstances[F[_], T](using inst: CoproductInstances[F, T]): inst.type = inst

  type ToUnion[T] = T match {
    case EmptyTuple => Nothing
    case a *: b => a | ToUnion[b]
  }

  type IndexOf[E, X] = IndexOf0[E, X, 0]

  type IndexOf0[E, X, I <: Int] <: Int = X match {
    case EmptyTuple => -1
    case x *: xs => x match {
      case E => I
      case _ => IndexOf0[E, xs, S[I]]
    }
  }

  type LiftP[F[_], T] <: Tuple = T match {
    case EmptyTuple => EmptyTuple
    case a *: b => F[a] *: LiftP[F, b]
  }

  inline def summonFirst[F[_], T, U]: F[U] = summonFirst0[LiftP[F, T]].asInstanceOf[F[U]]

  transparent inline def summonFirst0[T]: Any = inline erasedValue[T] match {
    case _: (a *: b) => summonFrom {
      case aa: `a` => aa
      case _ => summonFirst0[b]
    }
  }

  given Ops: Object with { // TODO: We probably don't need the given anymore.
    extension [T](gen: ProductGeneric[T])
      inline def toRepr(o: T): gen.MirroredElemTypes = Tuple.fromProduct(o.asInstanceOf).asInstanceOf[gen.MirroredElemTypes]
      inline def fromRepr(r: gen.MirroredElemTypes): T = gen.fromProduct(r.asInstanceOf).asInstanceOf[T]

    extension [T](gen: CoproductGeneric[T])
      inline def toRepr(o: T): ToUnion[gen.MirroredElemTypes] = o.asInstanceOf
      inline def fromRepr(r: ToUnion[gen.MirroredElemTypes]): T = r.asInstanceOf

    extension [F[_], T](gen: Generic[T])
      inline def derive(f: => (ProductGeneric[T] & gen.type) ?=> F[T], g: => (CoproductGeneric[T] & gen.type) ?=> F[T]): F[T] =
        inline gen match {
          case p: ProductGeneric[T]   => f(using p.asInstanceOf)
          case c: CoproductGeneric[T] => g(using c.asInstanceOf)
        }

    extension [F[_], T](inst: Instances[F, T])
      inline def map(x: T)(f: [t] => (F[t], t) => t): T = inst.erasedMap(x)(f.asInstanceOf).asInstanceOf

    extension [F[_], T](inst: ProductInstances[F, T])
      inline def construct(f: [t] => F[t] => t): T =
        inst.erasedConstruct(f.asInstanceOf).asInstanceOf
      inline def map2(x: T, y: T)(f: [t] => (F[t], t, t) => t): T =
        inst.erasedMap2(x, y)(f.asInstanceOf).asInstanceOf
      inline def unfold[Acc](i: Acc)(f: [t] => (Acc, F[t]) => (Acc, Option[t])): (Acc, Option[T]) =
        inst.erasedUnfold(i)(f.asInstanceOf).asInstanceOf
      inline def foldLeft[Acc](x: T)(i: Acc)(f: [t] => (Acc, F[t], t) => CompleteOr[Acc]): Acc =
        inst.erasedFoldLeft(x)(i)(f.asInstanceOf).asInstanceOf
      inline def foldLeft2[Acc](x: T, y: T)(i: Acc)(f: [t] => (Acc, F[t], t, t) => CompleteOr[Acc]): Acc =
        inst.erasedFoldLeft2(x, y)(i)(f.asInstanceOf).asInstanceOf

    extension [F[_], T](inst: CoproductInstances[F, T])
      inline def project[Acc](p: Int)(i: Acc)(f: [t] => (Acc, F[t]) => (Acc, Option[t])): (Acc, Option[T]) =
        inst.erasedProject(p)(i)(f.asInstanceOf).asInstanceOf
      inline def fold[R](x: T)(f: [t] => (F[t], t) => R): R =
        inst.erasedFold(x)(f.asInstanceOf).asInstanceOf
      inline def fold2[R](x: T, y: T)(a: => R)(f: [t] => (F[t], t, t) => R): R =
        inst.erasedFold2(x, y)(a.asInstanceOf)(f.asInstanceOf).asInstanceOf
  }

  inline given mkInstances[F[_], T](using gen: Generic[T]): Instances[F, T] =
    inline gen match {
      case p: ProductGeneric[T]   => mkProductInstances[F, T](using p)
      case c: CoproductGeneric[T] => mkCoproductInstances[F, T](using c)
    }

  inline given mkProductInstances[F[_], T](using gen: ProductGeneric[T]): ProductInstances[F, T] =
    ErasedProductInstances[K0.type, F[T], LiftP[F, gen.MirroredElemTypes]](gen)

  inline given mkCoproductInstances[F[_], T](using gen: CoproductGeneric[T]): CoproductInstances[F, T] =
    ErasedCoproductInstances[K0.type, F[T], LiftP[F, gen.MirroredElemTypes]](gen)
}

object K1 {
  type Kind[C, O[_]] = C { type Kind = K1.type ; type MirroredType[X] = O[X] ; type MirroredElemTypes[_] }
  type Generic[O[_]] = Kind[Mirror, O]
  type ProductGeneric[O[_]] = Kind[Mirror.Product, O]
  type CoproductGeneric[O[_]] = Kind[Mirror.Sum, O]

  def Generic[O[_]](using gen: Generic[O]): gen.type = gen
  def ProductGeneric[O[_]](using gen: ProductGeneric[O]): gen.type = gen
  def CoproductGeneric[O[_]](using gen: CoproductGeneric[O]): gen.type = gen

  type Instances[F[_[_]], T[_]] = ErasedInstances[K1.type, F[T]]
  type ProductInstances[F[_[_]], T[_]] = ErasedProductInstances[K1.type, F[T]]
  type CoproductInstances[F[_[_]], T[_]] = ErasedCoproductInstances[K1.type, F[T]]

  def Instances[F[_[_]], T[_]](using inst: Instances[F, T]): inst.type = inst
  def ProductInstances[F[_[_]], T[_]](using inst: ProductInstances[F, T]): inst.type = inst
  def CoproductInstances[F[_[_]], T[_]](using inst: CoproductInstances[F, T]): inst.type = inst

  class Dummy
  type Apply[T[_]] = T[Dummy]
  type Unapply[F[_[_]], T] = T match {
    case Wrap[Apply[a]] => F[a]
    case Wrap[Dummy] => F[Id]
    case Wrap[c] => F[Const[c]]
  }

  type LiftP[F[_[_]], T[_]] = LiftP0[F, Apply[T]]

  type LiftP0[F[_[_]], T] <: Tuple = T match {
    case EmptyTuple => EmptyTuple
    case (a *:  b) => Unapply[F, Wrap[a]] *: LiftP0[F, b]
  }

  inline def summonFirst[F[_[_]], T[_], U[_]]: F[U] = summonFirst0[LiftP[F, T]].asInstanceOf[F[U]]

  transparent inline def summonFirst0[T]: Any = inline erasedValue[T] match {
    case _: (a *: b) => summonFrom {
      case aa: `a` => aa
      case _ => summonFirst0[b]
    }
  }

  given Ops: Object with {
    extension [T[_], A] (gen: ProductGeneric[T])
      inline def toRepr(o: T[A]): gen.MirroredElemTypes[A] = Tuple.fromProduct(o.asInstanceOf).asInstanceOf[gen.MirroredElemTypes[A]]
      inline def fromRepr(r: gen.MirroredElemTypes[A]): T[A] = gen.fromProduct(r.asInstanceOf).asInstanceOf[T[A]]

    extension [T[_], A](gen: CoproductGeneric[T])
      inline def toRepr(o: T[A]): K0.ToUnion[gen.MirroredElemTypes[A]] = o.asInstanceOf
      inline def fromRepr(r: K0.ToUnion[gen.MirroredElemTypes[A]]): T[A] = r.asInstanceOf

    extension [F[_[_]], T[_]](gen: Generic[T]) inline def derive(f: => (ProductGeneric[T] & gen.type) ?=> F[T], g: => (CoproductGeneric[T] & gen.type) ?=> F[T]): F[T] =
      inline gen match {
        case p: ProductGeneric[T]   => f(using p.asInstanceOf)
        case c: CoproductGeneric[T] => g(using c.asInstanceOf)
      }

    extension [F[_[_]], T[_]](inst: Instances[F, T])
      inline def map[A, R](x: T[A])(f: [t[_]] => (F[t], t[A]) => t[R]): T[R] =
        inst.erasedMap(x)(f.asInstanceOf).asInstanceOf

    extension [F[_[_]], T[_]](inst: ProductInstances[F, T])
      inline def construct[R](f: [t[_]] => F[t] => t[R]): T[R] =
        inst.erasedConstruct(f.asInstanceOf).asInstanceOf
      inline def map2[A, B, R](x: T[A], y: T[B])(f: [t[_]] => (F[t], t[A], t[B]) => t[R]): T[R] =
        inst.erasedMap2(x, y)(f.asInstanceOf).asInstanceOf
      inline def foldLeft[A, Acc](x: T[A])(i: Acc)(f: [t[_]] => (Acc, F[t], t[A]) => CompleteOr[Acc]): Acc =
        inst.erasedFoldLeft(x)(i)(f.asInstanceOf).asInstanceOf
      inline def foldLeft2[A, B, Acc](x: T[A], y: T[B])(i: Acc)(f: [t[_]] => (Acc, F[t], t[A], t[B]) => CompleteOr[Acc]): Acc =
        inst.erasedFoldLeft2(x, y)(i)(f.asInstanceOf).asInstanceOf

    extension [F[_[_]], T[_]](inst: CoproductInstances[F, T])
      inline def fold[A, R](x: T[A])(f: [t[_]] => (F[t], t[A]) => R): R =
        inst.erasedFold(x)(f.asInstanceOf).asInstanceOf
      inline def fold2[A, B, R](x: T[A], y: T[B])(a: => R)(f: [t[_]] => (F[t], t[A], t[B]) => R): R =
        inst.erasedFold2(x, y)(a.asInstanceOf)(f.asInstanceOf).asInstanceOf
  }

  inline given mkInstances[F[_[_]], T[_]](using gen: Generic[T]): Instances[F, T] =
    inline gen match {
      case p: ProductGeneric[T] => mkProductInstances[F, T](using p)
      case c: CoproductGeneric[T] => mkCoproductInstances[F, T](using c)
    }

  inline given mkProductInstances[F[_[_]], T[_]](using gen: ProductGeneric[T]): ProductInstances[F, T] =
    ErasedProductInstances[K1.type, F[T], LiftP[F, gen.MirroredElemTypes]](gen)

  inline given mkCoproductInstances[F[_[_]], T[_]](using gen: CoproductGeneric[T]): CoproductInstances[F, T] =
    ErasedCoproductInstances[K1.type, F[T], LiftP[F, gen.MirroredElemTypes]](gen)
}

object K11 {
  type Kind[C, O[_[_]]] = C { type Kind = K11.type ; type MirroredType[X[_]] = O[X] ; type MirroredElemTypes[_[_]] }
  type Generic[O[_[_]]] = Kind[Mirror, O]
  type ProductGeneric[O[_[_]]] = Kind[Mirror.Product, O]
  type CoproductGeneric[O[_[_]]] = Kind[Mirror.Sum, O]

  def Generic[O[_[_]]](using gen: Generic[O]): gen.type = gen
  def ProductGeneric[O[_[_]]](using gen: ProductGeneric[O]): gen.type = gen
  def CoproductGeneric[O[_[_]]](using gen: CoproductGeneric[O]): gen.type = gen

  type Instances[F[_[_[_]]], T[_[_]]] = ErasedInstances[K11.type, F[T]]
  type ProductInstances[F[_[_[_]]], T[_[_]]] = ErasedProductInstances[K11.type, F[T]]
  type CoproductInstances[F[_[_[_]]], T[_[_]]] = ErasedCoproductInstances[K11.type, F[T]]

  def Instances[F[_[_[_]]], T[_[_]]](using inst: Instances[F, T]): inst.type = inst
  def ProductInstances[F[_[_[_]]], T[_[_]]](using inst: ProductInstances[F, T]): inst.type = inst
  def CoproductInstances[F[_[_[_]]], T[_[_]]](using inst: CoproductInstances[F, T]): inst.type = inst

  type Id[t] = [f[_]] =>> f[t]
  type Const[c] = [f[_]] =>> c

  class Dummy[T]
  type Apply[T[_[_]]] = T[Dummy]
  type Unapply[F[_[_[_]]], T] = T match {
    case Wrap[Apply[a]] => F[a]
    case Wrap[Dummy[a]] => F[Id[a]]
    case Wrap[c] => F[Const[c]]
  }

  type LiftP[F[_[_[_]]], T[_[_]]] = LiftP0[F, Apply[T]]

  type LiftP0[F[_[_[_]]], T] <: Tuple = T match {
    case EmptyTuple => EmptyTuple
    case (a *:  b) => Unapply[F, Wrap[a]] *: LiftP0[F, b]
  }

  inline def summonFirst[F[_[_[_]]], T[_[_]], U[_[_]]]: F[U] = summonFirst0[LiftP[F, T]].asInstanceOf[F[U]]

  transparent inline def summonFirst0[T]: Any = inline erasedValue[T] match {
    case _: (a *: b) => summonFrom {
      case aa: `a` => aa
      case _ => summonFirst0[b]
    }
  }

  given Ops: Object with {
    extension [T[_[_]], A[_]](gen: ProductGeneric[T])
      inline def toRepr(o: T[A]): gen.MirroredElemTypes[A] = Tuple.fromProduct(o.asInstanceOf).asInstanceOf[gen.MirroredElemTypes[A]]
      inline def fromRepr(r: gen.MirroredElemTypes[A]): T[A] = gen.fromProduct(r.asInstanceOf).asInstanceOf[T[A]]

    extension [T[_[_]], A[_]](gen: CoproductGeneric[T])
      inline def toRepr(o: T[A]): K0.ToUnion[gen.MirroredElemTypes[A]] = o.asInstanceOf
      inline def fromRepr(r: K0.ToUnion[gen.MirroredElemTypes[A]]): T[A] = r.asInstanceOf

    extension [F[_[_[_]]], T[_[_]]](gen: Generic[T]) inline def derive(f: => (ProductGeneric[T] & gen.type) ?=> F[T], g: => (CoproductGeneric[T] & gen.type) ?=> F[T]): F[T] =
      inline gen match {
        case p: ProductGeneric[T]   => f(using p.asInstanceOf)
        case c: CoproductGeneric[T] => g(using c.asInstanceOf)
      }

    extension [F[_[_[_]]], T[_[_]]](inst: Instances[F, T])
      inline def map[A[_], R[_]](x: T[A])(f: [t[_[_]]] => (F[t], t[A]) => t[R]): T[R] =
        inst.erasedMap(x)(f.asInstanceOf).asInstanceOf

    extension [F[_[_[_]]], T[_[_]]](inst: ProductInstances[F, T])
      inline def construct[R[_]](f: [t[_[_]]] => F[t] => t[R]): T[R] =
        inst.erasedConstruct(f.asInstanceOf).asInstanceOf
      inline def map2[A[_], B[_], R[_]](x: T[A], y: T[B])(f: [t[_[_]]] => (F[t], t[A], t[B]) => t[R]): T[R] =
        inst.erasedMap2(x, y)(f.asInstanceOf).asInstanceOf
      inline def foldLeft[A[_], Acc](x: T[A])(i: Acc)(f: [t[_[_]]] => (Acc, F[t], t[A]) => CompleteOr[Acc]): Acc =
        inst.erasedFoldLeft(x)(i)(f.asInstanceOf).asInstanceOf
      inline def foldLeft2[A[_], B[_], Acc](x: T[A], y: T[B])(i: Acc)(f: [t[_[_]]] => (Acc, F[t], t[A], t[B]) => CompleteOr[Acc]): Acc =
        inst.erasedFoldLeft2(x, y)(i)(f.asInstanceOf).asInstanceOf

    extension [F[_[_[_]]], T[_[_]]](inst: CoproductInstances[F, T]) 
      inline def fold[A[_], R](x: T[A])(f: [t[_[_]]] => (F[t], t[A]) => R): R =
        inst.erasedFold(x)(f.asInstanceOf).asInstanceOf
      inline def fold2[A[_], B[_], R](x: T[A], y: T[B])(a: => R)(f: [t[_[_]]] => (F[t], t[A], t[B]) => R): R =
        inst.erasedFold2(x, y)(a.asInstanceOf)(f.asInstanceOf).asInstanceOf
  }

  inline given mkInstances[F[_[_[_]]], T[_[_]]](using gen: Generic[T]): Instances[F, T] =
    inline gen match {
      case p: ProductGeneric[T]   => mkProductInstances[F, T](using p)
      case c: CoproductGeneric[T] => mkCoproductInstances[F, T](using c)
    }

  inline given mkProductInstances[F[_[_[_]]], T[_[_]]](using gen: ProductGeneric[T]): ProductInstances[F, T] =
    ErasedProductInstances[K11.type, F[T], LiftP[F, gen.MirroredElemTypes]](gen)

  inline given mkCoproductInstances[F[_[_[_]]], T[_[_]]](using gen: CoproductGeneric[T]): CoproductInstances[F, T] =
    ErasedCoproductInstances[K11.type, F[T], LiftP[F, gen.MirroredElemTypes]](gen)
}

object K2 {
  type Kind[C, O[_, _]] = C { type Kind = K2.type ; type MirroredType[X, Y] = O[X, Y] ; type MirroredElemTypes[_, _] }
  type Generic[O[_, _]] = Kind[Mirror, O]
  type ProductGeneric[O[_, _]] = Kind[Mirror.Product, O]
  type CoproductGeneric[O[_, _]] = Kind[Mirror.Sum, O]

  def Generic[O[_, _]](using gen: Generic[O]): gen.type = gen
  def ProductGeneric[O[_, _]](using gen: ProductGeneric[O]): gen.type = gen
  def CoproductGeneric[O[_, _]](using gen: CoproductGeneric[O]): gen.type = gen

  type Instances[F[_[_, _]], T[_, _]] = ErasedInstances[K2.type, F[T]]
  type ProductInstances[F[_[_, _]], T[_, _]] = ErasedProductInstances[K2.type, F[T]]
  type CoproductInstances[F[_[_, _]], T[_, _]] = ErasedCoproductInstances[K2.type, F[T]]

  def Instances[F[_[_, _]], T[_, _]](using inst: Instances[F, T]): inst.type = inst
  def ProductInstances[F[_[_, _]], T[_, _]](using inst: ProductInstances[F, T]): inst.type = inst
  def CoproductInstances[F[_[_, _]], T[_, _]](using inst: CoproductInstances[F, T]): inst.type = inst

  type Id1[t, u] = t
  type Id2[t, u] = u
  type Const[c] = [t, u] =>> c

  class Dummy1
  class Dummy2
  type Apply[T[_, _]] = T[Dummy1, Dummy2]
  type Unapply[F[_[_, _]], T] = T match {
    case Wrap[Apply[a]] => F[a]
    case Wrap[Dummy1] => F[Id1]
    case Wrap[Dummy2] => F[Id2]
    case Wrap[c] => F[Const[c]]
  }

  type LiftP[F[_[_, _]], T[_, _]] = LiftP0[F, Apply[T]]

  type LiftP0[F[_[_, _]], T] <: Tuple = T match {
    case EmptyTuple => EmptyTuple
    case (a *:  b) => Unapply[F, Wrap[a]] *: LiftP0[F, b]
  }

  inline def summonFirst[F[_[_, _]], T[_, _], U[_, _]]: F[U] = summonFirst0[LiftP[F, T]].asInstanceOf[F[U]]

  transparent inline def summonFirst0[T]: Any = inline erasedValue[T] match {
    case _: (a *: b) => summonFrom {
      case aa: `a` => aa
      case _ => summonFirst0[b]
    }
  }

  given Ops: Object with {
    extension [T[_, _], A, B](gen: ProductGeneric[T]) inline def toRepr(o: T[A, B]): gen.MirroredElemTypes[A, B] = Tuple.fromProduct(o.asInstanceOf).asInstanceOf[gen.MirroredElemTypes[A, B]]
    extension [T[_, _], A, B](gen: ProductGeneric[T]) inline def fromRepr(r: gen.MirroredElemTypes[A, B]): T[A, B] = gen.fromProduct(r.asInstanceOf).asInstanceOf[T[A, B]]

    extension [T[_, _], A, B](gen: CoproductGeneric[T]) inline def toRepr(o: T[A, B]): K0.ToUnion[gen.MirroredElemTypes[A, B]] = o.asInstanceOf
    extension [T[_, _], A, B](gen: CoproductGeneric[T]) inline def fromRepr(r: K0.ToUnion[gen.MirroredElemTypes[A, B]]): T[A, B] = r.asInstanceOf

    extension [F[_[_, _]], T[_, _]](gen: Generic[T]) inline def derive(f: => (ProductGeneric[T] & gen.type) ?=> F[T], g: => (CoproductGeneric[T] & gen.type) ?=> F[T]): F[T] =
      inline gen match {
        case p: ProductGeneric[T]   => f(using p.asInstanceOf)
        case c: CoproductGeneric[T] => g(using c.asInstanceOf)
      }

    extension [F[_[_, _]], T[_, _]](inst: Instances[F, T])
      inline def map[A, B, R, S](x: T[A, B])(f: [t[_, _]] => (F[t], t[A, B]) => t[R, S]): T[R, S] =
        inst.erasedMap(x)(f.asInstanceOf).asInstanceOf

    extension [F[_[_, _]], T[_, _]](inst: ProductInstances[F, T])
      inline def construct[R, S](f: [t[_, _]] => F[t] => t[R, S]): T[R, S] =
        inst.erasedConstruct(f.asInstanceOf).asInstanceOf
      inline def map2[A, B, C, D, R, S](x: T[A, B], y: T[C, D])(f: [t[_, _]] => (F[t], t[A, B], t[C, D]) => t[R, S]): T[R, S] =
        inst.erasedMap2(x, y)(f.asInstanceOf).asInstanceOf
      inline def foldLeft[A, B, Acc](x: T[A, B])(i: Acc)(f: [t[_, _]] => (Acc, F[t], t[A, B]) => CompleteOr[Acc]): Acc =
        inst.erasedFoldLeft(x)(i)(f.asInstanceOf).asInstanceOf
      inline def foldLeft2[A, B, C, D, Acc](x: T[A, B], y: T[C, D])(i: Acc)(f: [t[_, _]] => (Acc, F[t], t[A, B], t[C, D]) => CompleteOr[Acc]): Acc =
        inst.erasedFoldLeft2(x, y)(i)(f.asInstanceOf).asInstanceOf

    extension [F[_[_, _]], T[_, _]](inst: CoproductInstances[F, T])
      inline def fold[A, B, R](x: T[A, B])(f: [t[_, _]] => (F[t], t[A, B]) => R): R =
        inst.erasedFold(x)(f.asInstanceOf).asInstanceOf
      inline def fold2[A, B, C, D, R](x: T[A, B], y: T[C, D])(a: => R)(f: [t[_, _]] => (F[t], t[A, B], t[C, D]) => R): R =
        inst.erasedFold2(x, y)(a.asInstanceOf)(f.asInstanceOf).asInstanceOf
  }

  inline given mkInstances[F[_[_, _]], T[_, _]](using gen: Generic[T]): Instances[F, T] =
    inline gen match {
      case p: ProductGeneric[T]   => mkProductInstances[F, T](using p)
      case c: CoproductGeneric[T] => mkCoproductInstances[F, T](using c)
    }

  inline given mkProductInstances[F[_[_, _]], T[_, _]](using gen: ProductGeneric[T]): ProductInstances[F, T] =
    ErasedProductInstances[K2.type, F[T], LiftP[F, gen.MirroredElemTypes]](gen)

  inline given mkCoproductInstances[F[_[_, _]], T[_, _]](using gen: CoproductGeneric[T]): CoproductInstances[F, T] =
    ErasedCoproductInstances[K2.type, F[T], LiftP[F, gen.MirroredElemTypes]](gen)
}

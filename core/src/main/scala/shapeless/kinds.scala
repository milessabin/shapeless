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

package shapeless

import scala.compiletime._
import scala.deriving._

object K0 {
  type Kind[C, O] = C { type Kind = K0.type ; type MirroredType = O ; type MirroredElemTypes }
  type Generic[O] = Kind[Mirror, O]
  type ProductGeneric[O] = Kind[Mirror.Product, O]
  type CoproductGeneric[O] = Kind[Mirror.Sum, O]

  def Generic[O](given gen: Generic[O]): gen.type = gen
  def ProductGeneric[O](given gen: ProductGeneric[O]): gen.type = gen
  def CoproductGeneric[O](given gen: CoproductGeneric[O]): gen.type = gen

  type Instances[F[_], T] = ErasedInstances[K0.type, F[T]]
  type ProductInstances[F[_], T] = ErasedProductInstances[K0.type, F[T]]
  type CoproductInstances[F[_], T] = ErasedCoproductInstances[K0.type, F[T]]

  def Instances[F[_], T](given inst: Instances[F, T]): inst.type = inst
  def ProductInstances[F[_], T](given inst: ProductInstances[F, T]): inst.type = inst
  def CoproductInstances[F[_], T](given inst: CoproductInstances[F, T]): inst.type = inst

  type ToUnion[T] = T match {
    case Unit => Nothing
    case a *: b => a | ToUnion[b]
  }

  type IndexOf[E, X] = IndexOf0[E, X, 0]

  type IndexOf0[E, X, I <: Int] <: Int = X match {
    case Unit => -1
    case x *: xs => x match {
      case E => I
      case _ => IndexOf0[E, xs, S[I]]
    }
  }

  type LiftP[F[_], T] <: Tuple = T match {
    case Unit => Unit
    case a *: b => F[a] *: LiftP[F, b]
  }

  inline def summonFirst[F[_], T, U]: F[U] = summonFirst0[LiftP[F, T]].asInstanceOf[F[U]]

  inline def summonFirst0[T] <: Any = inline erasedValue[T] match {
    case _: (a *: b) => summonFrom {
      case aa: `a` => aa
      case _ => summonFirst0[b]
    }
  }

  given Ops: Object {
    inline def [T](gen: ProductGeneric[T]) toRepr(o: T): gen.MirroredElemTypes = Tuple.fromProduct(o.asInstanceOf).asInstanceOf[gen.MirroredElemTypes]
    inline def [T](gen: ProductGeneric[T]) fromRepr(r: gen.MirroredElemTypes): T = gen.fromProduct(r.asInstanceOf).asInstanceOf[T]

    inline def [T](gen: CoproductGeneric[T]) toRepr(o: T): ToUnion[gen.MirroredElemTypes] = o.asInstanceOf
    inline def [T](gen: CoproductGeneric[T]) fromRepr(r: ToUnion[gen.MirroredElemTypes]): T = r.asInstanceOf

    inline def [F[_], T](gen: Generic[T]) derive(f: => (given ProductGeneric[T] & gen.type) => F[T], g: => (given CoproductGeneric[T] & gen.type) => F[T]): F[T] =
      inline gen match {
        case p: ProductGeneric[T]   => f(given p.asInstanceOf)
        case c: CoproductGeneric[T] => g(given c.asInstanceOf)
      }

    inline def [F[_], T](inst: Instances[F, T]) map(x: T)(f: [t] => (F[t], t) => t): T =
      inst.erasedMap(x)(f.asInstanceOf).asInstanceOf

    inline def [F[_], T](inst: ProductInstances[F, T]) construct(f: [t] => F[t] => t): T =
      inst.erasedConstruct(f.asInstanceOf).asInstanceOf
    inline def [F[_], T, Acc](inst: ProductInstances[F, T]) unfold(i: Acc)(f: [t] => (Acc, F[t]) => (Acc, Option[t])): (Acc, Option[T]) =
      inst.erasedUnfold(i)(f.asInstanceOf).asInstanceOf
    inline def [F[_], T](inst: ProductInstances[F, T]) map2(x: T, y: T)(f: [t] => (F[t], t, t) => t): T =
      inst.erasedMap2(x, y)(f.asInstanceOf).asInstanceOf
    inline def [F[_], T, Acc](inst: ProductInstances[F, T]) foldLeft(x: T)(i: Acc)(f: [t] => (Acc, F[t], t) => CompleteOr[Acc]): Acc =
      inst.erasedFoldLeft(x)(i)(f.asInstanceOf).asInstanceOf
    inline def [F[_], T, Acc](inst: ProductInstances[F, T]) foldLeft2(x: T, y: T)(i: Acc)(f: [t] => (Acc, F[t], t, t) => CompleteOr[Acc]): Acc =
      inst.erasedFoldLeft2(x, y)(i)(f.asInstanceOf).asInstanceOf

    inline def [F[_], T, Acc](inst: CoproductInstances[F, T]) project(p: Int)(i: Acc)(f: [t] => (Acc, F[t]) => (Acc, Option[t])): (Acc, Option[T]) =
      inst.erasedProject(p)(i)(f.asInstanceOf).asInstanceOf
    inline def [F[_], T, R](inst: CoproductInstances[F, T]) fold(x: T)(f: [t] => (F[t], t) => R): R =
      inst.erasedFold(x)(f.asInstanceOf).asInstanceOf
    inline def [F[_], T, R](inst: CoproductInstances[F, T]) fold2(x: T, y: T)(a: => R)(f: [t] => (F[t], t, t) => R): R =
      inst.erasedFold2(x, y)(a.asInstanceOf)(f.asInstanceOf).asInstanceOf
  }

  inline given mkInstances[F[_], T](given gen: Generic[T]): Instances[F, T] =
    inline gen match {
      case p: ProductGeneric[T]   => mkProductInstances[F, T](given p)
      case c: CoproductGeneric[T] => mkCoproductInstances[F, T](given c)
    }

  inline given mkProductInstances[F[_], T](given gen: ProductGeneric[T]): ProductInstances[F, T] =
    ErasedProductInstances[K0.type, F[T], LiftP[F, gen.MirroredElemTypes]](gen)

  inline given mkCoproductInstances[F[_], T](given gen: CoproductGeneric[T]): CoproductInstances[F, T] =
    ErasedCoproductInstances[K0.type, F[T], LiftP[F, gen.MirroredElemTypes]](gen)
}

object K1 {
  type Kind[C, O[_]] = C { type Kind = K1.type ; type MirroredType = O ; type MirroredElemTypes[_] }
  type Generic[O[_]] = Kind[Mirror, O]
  type ProductGeneric[O[_]] = Kind[Mirror.Product, O]
  type CoproductGeneric[O[_]] = Kind[Mirror.Sum, O]

  def Generic[O[_]](given gen: Generic[O]): gen.type = gen
  def ProductGeneric[O[_]](given gen: ProductGeneric[O]): gen.type = gen
  def CoproductGeneric[O[_]](given gen: CoproductGeneric[O]): gen.type = gen

  type Instances[F[_[_]], T[_]] = ErasedInstances[K1.type, F[T]]
  type ProductInstances[F[_[_]], T[_]] = ErasedProductInstances[K1.type, F[T]]
  type CoproductInstances[F[_[_]], T[_]] = ErasedCoproductInstances[K1.type, F[T]]

  def Instances[F[_[_]], T[_]](given inst: Instances[F, T]): inst.type = inst
  def ProductInstances[F[_[_]], T[_]](given inst: ProductInstances[F, T]): inst.type = inst
  def CoproductInstances[F[_[_]], T[_]](given inst: CoproductInstances[F, T]): inst.type = inst

  class Dummy
  type Apply[T[_]] = T[Dummy]
  type Unapply[F[_[_]], T] = T match {
    case Wrap[Apply[a]] => F[a]
    case Wrap[Dummy] => F[Id]
    case Wrap[c] => F[Const[c]]
  }

  type LiftP[F[_[_]], T[_]] = LiftP0[F, Apply[T]]

  type LiftP0[F[_[_]], T] <: Tuple = T match {
    case Unit => Unit
    case (a *:  b) => Unapply[F, Wrap[a]] *: LiftP0[F, b]
  }

  inline def summonFirst[F[_[_]], T[_], U[_]]: F[U] = summonFirst0[LiftP[F, T]].asInstanceOf[F[U]]

  inline def summonFirst0[T] <: Any = inline erasedValue[T] match {
    case _: (a *: b) => summonFrom {
      case aa: `a` => aa
      case _ => summonFirst0[b]
    }
  }

  given Ops: Object {
    inline def [T[_], A](gen: ProductGeneric[T]) toRepr(o: T[A]): gen.MirroredElemTypes[A] = Tuple.fromProduct(o.asInstanceOf).asInstanceOf[gen.MirroredElemTypes[A]]
    inline def [T[_], A](gen: ProductGeneric[T]) fromRepr(r: gen.MirroredElemTypes[A]): T[A] = gen.fromProduct(r.asInstanceOf).asInstanceOf[T[A]]

    inline def [T[_], A](gen: CoproductGeneric[T]) toRepr(o: T[A]): K0.ToUnion[gen.MirroredElemTypes[A]] = o.asInstanceOf
    inline def [T[_], A](gen: CoproductGeneric[T]) fromRepr(r: K0.ToUnion[gen.MirroredElemTypes[A]]): T[A] = r.asInstanceOf

    inline def [F[_[_]], T[_]](gen: Generic[T]) derive(f: => (given ProductGeneric[T] & gen.type) => F[T], g: => (given CoproductGeneric[T] & gen.type) => F[T]): F[T] =
      inline gen match {
        case p: ProductGeneric[T]   => f(given p.asInstanceOf)
        case c: CoproductGeneric[T] => g(given c.asInstanceOf)
      }

    inline def [F[_[_]], T[_], A, R](inst: Instances[F, T]) map(x: T[A])(f: [t[_]] => (F[t], t[A]) => t[R]): T[R] =
      inst.erasedMap(x)(f.asInstanceOf).asInstanceOf

    inline def [F[_[_]], T[_], R](inst: ProductInstances[F, T]) construct(f: [t[_]] => F[t] => t[R]): T[R] =
      inst.erasedConstruct(f.asInstanceOf).asInstanceOf
    inline def [F[_[_]], T[_], A, B, R](inst: ProductInstances[F, T]) map2(x: T[A], y: T[B])(f: [t[_]] => (F[t], t[A], t[B]) => t[R]): T[R] =
      inst.erasedMap2(x, y)(f.asInstanceOf).asInstanceOf
    inline def [F[_[_]], T[_], A, Acc](inst: ProductInstances[F, T]) foldLeft(x: T[A])(i: Acc)(f: [t[_]] => (Acc, F[t], t[A]) => CompleteOr[Acc]): Acc =
      inst.erasedFoldLeft(x)(i)(f.asInstanceOf).asInstanceOf
    inline def [F[_[_]], T[_], A, B, Acc](inst: ProductInstances[F, T]) foldLeft2(x: T[A], y: T[B])(i: Acc)(f: [t[_]] => (Acc, F[t], t[A], t[B]) => CompleteOr[Acc]): Acc =
      inst.erasedFoldLeft2(x, y)(i)(f.asInstanceOf).asInstanceOf

    inline def [F[_[_]], T[_], A, R](inst: CoproductInstances[F, T]) fold(x: T[A])(f: [t[_]] => (F[t], t[A]) => R): R =
      inst.erasedFold(x)(f.asInstanceOf).asInstanceOf
    inline def [F[_[_]], T[_], A, B, R](inst: CoproductInstances[F, T]) fold2(x: T[A], y: T[B])(a: => R)(f: [t[_]] => (F[t], t[A], t[B]) => R): R =
      inst.erasedFold2(x, y)(a.asInstanceOf)(f.asInstanceOf).asInstanceOf
  }

  inline given mkInstances[F[_[_]], T[_]](given gen: Generic[T]): Instances[F, T] =
    inline gen match {
      case p: ProductGeneric[T] => mkProductInstances[F, T](given p)
      case c: CoproductGeneric[T] => mkCoproductInstances[F, T](given c)
    }

  inline given mkProductInstances[F[_[_]], T[_]](given gen: ProductGeneric[T]): ProductInstances[F, T] =
    ErasedProductInstances[K1.type, F[T], LiftP[F, gen.MirroredElemTypes]](gen)

  inline given mkCoproductInstances[F[_[_]], T[_]](given gen: CoproductGeneric[T]): CoproductInstances[F, T] =
    ErasedCoproductInstances[K1.type, F[T], LiftP[F, gen.MirroredElemTypes]](gen)
}

object K11 {
  type Kind[C, O[_[_]]] = C { type Kind = K11.type ; type MirroredType = O ; type MirroredElemTypes[_[_]] }
  type Generic[O[_[_]]] = Kind[Mirror, O]
  type ProductGeneric[O[_[_]]] = Kind[Mirror.Product, O]
  type CoproductGeneric[O[_[_]]] = Kind[Mirror.Sum, O]

  def Generic[O[_[_]]](given gen: Generic[O]): gen.type = gen
  def ProductGeneric[O[_[_]]](given gen: ProductGeneric[O]): gen.type = gen
  def CoproductGeneric[O[_[_]]](given gen: CoproductGeneric[O]): gen.type = gen

  type Instances[F[_[_[_]]], T[_[_]]] = ErasedInstances[K11.type, F[T]]
  type ProductInstances[F[_[_[_]]], T[_[_]]] = ErasedProductInstances[K11.type, F[T]]
  type CoproductInstances[F[_[_[_]]], T[_[_]]] = ErasedCoproductInstances[K11.type, F[T]]

  def Instances[F[_[_[_]]], T[_[_]]](given inst: Instances[F, T]): inst.type = inst
  def ProductInstances[F[_[_[_]]], T[_[_]]](given inst: ProductInstances[F, T]): inst.type = inst
  def CoproductInstances[F[_[_[_]]], T[_[_]]](given inst: CoproductInstances[F, T]): inst.type = inst

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
    case Unit => Unit
    case (a *:  b) => Unapply[F, Wrap[a]] *: LiftP0[F, b]
  }

  inline def summonFirst[F[_[_[_]]], T[_[_]], U[_[_]]]: F[U] = summonFirst0[LiftP[F, T]].asInstanceOf[F[U]]

  inline def summonFirst0[T] <: Any = inline erasedValue[T] match {
    case _: (a *: b) => summonFrom {
      case aa: `a` => aa
      case _ => summonFirst0[b]
    }
  }

  given Ops: Object {
    inline def [T[_[_]], A[_]](gen: ProductGeneric[T]) toRepr(o: T[A]): gen.MirroredElemTypes[A] = Tuple.fromProduct(o.asInstanceOf).asInstanceOf[gen.MirroredElemTypes[A]]
    inline def [T[_[_]], A[_]](gen: ProductGeneric[T]) fromRepr(r: gen.MirroredElemTypes[A]): T[A] = gen.fromProduct(r.asInstanceOf).asInstanceOf[T[A]]

    inline def [T[_[_]], A[_]](gen: CoproductGeneric[T]) toRepr(o: T[A]): K0.ToUnion[gen.MirroredElemTypes[A]] = o.asInstanceOf
    inline def [T[_[_]], A[_]](gen: CoproductGeneric[T]) fromRepr(r: K0.ToUnion[gen.MirroredElemTypes[A]]): T[A] = r.asInstanceOf

    inline def [F[_[_[_]]], T[_[_]]](gen: Generic[T]) derive(f: => (given ProductGeneric[T] & gen.type) => F[T], g: => (given CoproductGeneric[T] & gen.type) => F[T]): F[T] =
      inline gen match {
        case p: ProductGeneric[T]   => f(given p.asInstanceOf)
        case c: CoproductGeneric[T] => g(given c.asInstanceOf)
      }

    inline def [F[_[_[_]]], T[_[_]], A[_], R[_]](inst: Instances[F, T]) map(x: T[A])(f: [t[_[_]]] => (F[t], t[A]) => t[R]): T[R] =
      inst.erasedMap(x)(f.asInstanceOf).asInstanceOf

    inline def [F[_[_[_]]], T[_[_]], R[_]](inst: ProductInstances[F, T]) construct(f: [t[_[_]]] => F[t] => t[R]): T[R] =
      inst.erasedConstruct(f.asInstanceOf).asInstanceOf
    inline def [F[_[_[_]]], T[_[_]], A[_], B[_], R[_]](inst: ProductInstances[F, T]) map2(x: T[A], y: T[B])(f: [t[_[_]]] => (F[t], t[A], t[B]) => t[R]): T[R] =
      inst.erasedMap2(x, y)(f.asInstanceOf).asInstanceOf
    inline def [F[_[_[_]]], T[_[_]], A[_], Acc](inst: ProductInstances[F, T]) foldLeft(x: T[A])(i: Acc)(f: [t[_[_]]] => (Acc, F[t], t[A]) => CompleteOr[Acc]): Acc =
      inst.erasedFoldLeft(x)(i)(f.asInstanceOf).asInstanceOf
    inline def [F[_[_[_]]], T[_[_]], A[_], B[_], Acc](inst: ProductInstances[F, T]) foldLeft2(x: T[A], y: T[B])(i: Acc)(f: [t[_[_]]] => (Acc, F[t], t[A], t[B]) => CompleteOr[Acc]): Acc =
      inst.erasedFoldLeft2(x, y)(i)(f.asInstanceOf).asInstanceOf

    inline def [F[_[_[_]]], T[_[_]], A[_], R](inst: CoproductInstances[F, T]) fold(x: T[A])(f: [t[_[_]]] => (F[t], t[A]) => R): R =
      inst.erasedFold(x)(f.asInstanceOf).asInstanceOf
    inline def [F[_[_[_]]], T[_[_]], A[_], B[_], R](inst: CoproductInstances[F, T]) fold2(x: T[A], y: T[B])(a: => R)(f: [t[_[_]]] => (F[t], t[A], t[B]) => R): R =
      inst.erasedFold2(x, y)(a.asInstanceOf)(f.asInstanceOf).asInstanceOf
  }

  inline given mkInstances[F[_[_[_]]], T[_[_]]](given gen: Generic[T]): Instances[F, T] =
    inline gen match {
      case p: ProductGeneric[T]   => mkProductInstances[F, T](given p)
      case c: CoproductGeneric[T] => mkCoproductInstances[F, T](given c)
    }

  inline given mkProductInstances[F[_[_[_]]], T[_[_]]](given gen: ProductGeneric[T]): ProductInstances[F, T] =
    ErasedProductInstances[K11.type, F[T], LiftP[F, gen.MirroredElemTypes]](gen)

  inline given mkCoproductInstances[F[_[_[_]]], T[_[_]]](given gen: CoproductGeneric[T]): CoproductInstances[F, T] =
    ErasedCoproductInstances[K11.type, F[T], LiftP[F, gen.MirroredElemTypes]](gen)
}

object K2 {
  type Kind[C, O[_, _]] = C { type Kind = K2.type ; type MirroredType = O ; type MirroredElemTypes[_, _] }
  type Generic[O[_, _]] = Kind[Mirror, O]
  type ProductGeneric[O[_, _]] = Kind[Mirror.Product, O]
  type CoproductGeneric[O[_, _]] = Kind[Mirror.Sum, O]

  def Generic[O[_, _]](given gen: Generic[O]): gen.type = gen
  def ProductGeneric[O[_, _]](given gen: ProductGeneric[O]): gen.type = gen
  def CoproductGeneric[O[_, _]](given gen: CoproductGeneric[O]): gen.type = gen

  type Instances[F[_[_, _]], T[_, _]] = ErasedInstances[K2.type, F[T]]
  type ProductInstances[F[_[_, _]], T[_, _]] = ErasedProductInstances[K2.type, F[T]]
  type CoproductInstances[F[_[_, _]], T[_, _]] = ErasedCoproductInstances[K2.type, F[T]]

  def Instances[F[_[_, _]], T[_, _]](given inst: Instances[F, T]): inst.type = inst
  def ProductInstances[F[_[_, _]], T[_, _]](given inst: ProductInstances[F, T]): inst.type = inst
  def CoproductInstances[F[_[_, _]], T[_, _]](given inst: CoproductInstances[F, T]): inst.type = inst

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
    case Unit => Unit
    case (a *:  b) => Unapply[F, Wrap[a]] *: LiftP0[F, b]
  }

  inline def summonFirst[F[_[_, _]], T[_, _], U[_, _]]: F[U] = summonFirst0[LiftP[F, T]].asInstanceOf[F[U]]

  inline def summonFirst0[T] <: Any = inline erasedValue[T] match {
    case _: (a *: b) => summonFrom {
      case aa: `a` => aa
      case _ => summonFirst0[b]
    }
  }

  given Ops: Object {
    inline def [T[_, _], A, B](gen: ProductGeneric[T]) toRepr(o: T[A, B]): gen.MirroredElemTypes[A, B] = Tuple.fromProduct(o.asInstanceOf).asInstanceOf[gen.MirroredElemTypes[A, B]]
    inline def [T[_, _], A, B](gen: ProductGeneric[T]) fromRepr(r: gen.MirroredElemTypes[A, B]): T[A, B] = gen.fromProduct(r.asInstanceOf).asInstanceOf[T[A, B]]

    inline def [T[_, _], A, B](gen: CoproductGeneric[T]) toRepr(o: T[A, B]): K0.ToUnion[gen.MirroredElemTypes[A, B]] = o.asInstanceOf
    inline def [T[_, _], A, B](gen: CoproductGeneric[T]) fromRepr(r: K0.ToUnion[gen.MirroredElemTypes[A, B]]): T[A, B] = r.asInstanceOf

    inline def [F[_[_, _]], T[_, _]](gen: Generic[T]) derive(f: => (given ProductGeneric[T] & gen.type) => F[T], g: => (given CoproductGeneric[T] & gen.type) => F[T]): F[T] =
      inline gen match {
        case p: ProductGeneric[T]   => f(given p.asInstanceOf)
        case c: CoproductGeneric[T] => g(given c.asInstanceOf)
      }

    inline def [F[_[_, _]], T[_, _], A, B, R, S](inst: Instances[F, T]) map(x: T[A, B])(f: [t[_, _]] => (F[t], t[A, B]) => t[R, S]): T[R, S] =
      inst.erasedMap(x)(f.asInstanceOf).asInstanceOf

    inline def [F[_[_, _]], T[_, _], R, S](inst: ProductInstances[F, T]) construct(f: [t[_, _]] => F[t] => t[R, S]): T[R, S] =
      inst.erasedConstruct(f.asInstanceOf).asInstanceOf
    inline def [F[_[_, _]], T[_, _], A, B, C, D, R, S](inst: ProductInstances[F, T]) map2(x: T[A, B], y: T[C, D])(f: [t[_, _]] => (F[t], t[A, B], t[C, D]) => t[R, S]): T[R, S] =
      inst.erasedMap2(x, y)(f.asInstanceOf).asInstanceOf
    inline def [F[_[_, _]], T[_, _], A, B, Acc](inst: ProductInstances[F, T]) foldLeft(x: T[A, B])(i: Acc)(f: [t[_, _]] => (Acc, F[t], t[A, B]) => CompleteOr[Acc]): Acc =
      inst.erasedFoldLeft(x)(i)(f.asInstanceOf).asInstanceOf
    inline def [F[_[_, _]], T[_, _], A, B, C, D, Acc](inst: ProductInstances[F, T]) foldLeft2(x: T[A, B], y: T[C, D])(i: Acc)(f: [t[_, _]] => (Acc, F[t], t[A, B], t[C, D]) => CompleteOr[Acc]): Acc =
      inst.erasedFoldLeft2(x, y)(i)(f.asInstanceOf).asInstanceOf

    inline def [F[_[_, _]], T[_, _], A, B, R](inst: CoproductInstances[F, T]) fold(x: T[A, B])(f: [t[_, _]] => (F[t], t[A, B]) => R): R =
      inst.erasedFold(x)(f.asInstanceOf).asInstanceOf
    inline def [F[_[_, _]], T[_, _], A, B, C, D, R](inst: CoproductInstances[F, T]) fold2(x: T[A, B], y: T[C, D])(a: => R)(f: [t[_, _]] => (F[t], t[A, B], t[C, D]) => R): R =
      inst.erasedFold2(x, y)(a.asInstanceOf)(f.asInstanceOf).asInstanceOf
  }

  inline given mkInstances[F[_[_, _]], T[_, _]](given gen: Generic[T]): Instances[F, T] =
    inline gen match {
      case p: ProductGeneric[T]   => mkProductInstances[F, T](given p)
      case c: CoproductGeneric[T] => mkCoproductInstances[F, T](given c)
    }

  inline given mkProductInstances[F[_[_, _]], T[_, _]](given gen: ProductGeneric[T]): ProductInstances[F, T] =
    ErasedProductInstances[K2.type, F[T], LiftP[F, gen.MirroredElemTypes]](gen)

  inline given mkCoproductInstances[F[_[_, _]], T[_, _]](given gen: CoproductGeneric[T]): CoproductInstances[F, T] =
    ErasedCoproductInstances[K2.type, F[T], LiftP[F, gen.MirroredElemTypes]](gen)
}

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
  type Generic[O] = Mirror { type MirroredType = O ; type MirroredElemTypes }
  type ProductGeneric[O] = Mirror.Product { type MirroredType = O ; type MirroredElemTypes }
  type CoproductGeneric[O] = Mirror.Sum { type MirroredType = O ; type MirroredElemTypes }

  def Generic[O] given (gen: Generic[O]): Generic[O] { type MirroredElemTypes = gen.MirroredElemTypes ; type MirroredLabel = gen.MirroredLabel ; type MirroredElemLabels = gen.MirroredElemLabels } = gen
  def ProductGeneric[O] given (gen: ProductGeneric[O]): ProductGeneric[O] { type MirroredElemTypes = gen.MirroredElemTypes ; type MirroredLabel = gen.MirroredLabel ; type MirroredElemLabels = gen.MirroredElemLabels } = gen
  def CoproductGeneric[O] given (gen: CoproductGeneric[O]): CoproductGeneric[O] { type MirroredElemTypes = gen.MirroredElemTypes ; type MirroredLabel = gen.MirroredLabel ; type MirroredElemLabels = gen.MirroredElemLabels } = gen

  type Instances[F[_], T] = ErasedInstances[F[T]]
  type ProductInstances[F[_], T] = ErasedProductInstances[F[T]]
  type CoproductInstances[F[_], T] = ErasedCoproductInstances[F[T]]

  def Instances[F[_], T] given (inst: Instances[F, T]): inst.type = inst
  def ProductInstances[F[_], T] given (inst: ProductInstances[F, T]): inst.type = inst
  def CoproductInstances[F[_], T] given (inst: CoproductInstances[F, T]): inst.type = inst

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
    case _: (a *: b) => implicit match {
      case aa: `a` => aa
      case _ => summonFirst0[b]
    }
  }

  given Ops {
    inline def (gen: ProductGeneric[Obj]) toRepr [Obj] (o: Obj): gen.MirroredElemTypes = Tuple.fromProduct(o.asInstanceOf).asInstanceOf[gen.MirroredElemTypes]
    inline def (gen: ProductGeneric[Obj]) fromRepr [Obj] (r: gen.MirroredElemTypes): Obj = gen.fromProduct(r.asInstanceOf).asInstanceOf[Obj]

    inline def (gen: CoproductGeneric[Obj]) toRepr [Obj] (o: Obj): ToUnion[gen.MirroredElemTypes] = o.asInstanceOf
    inline def (gen: CoproductGeneric[Obj]) fromRepr [Obj] (r: ToUnion[gen.MirroredElemTypes]): Obj = r.asInstanceOf

    inline def (inst: Instances[F, T]) map [F[_], T] (x: T)(f: [t] => (F[t], t) => t): T =
      inst.erasedMap(x)(f.asInstanceOf).asInstanceOf

    inline def (inst: ProductInstances[F, T]) construct [F[_], T] (f: [t] => F[t] => t): T =
      inst.erasedConstruct(f.asInstanceOf).asInstanceOf
    inline def (inst: ProductInstances[F, T]) unfold [F[_], T, Acc] (i: Acc)(f: [t] => (Acc, F[t]) => (Acc, Option[t])): (Acc, Option[T]) =
      inst.erasedUnfold(i)(f.asInstanceOf).asInstanceOf
    inline def (inst: ProductInstances[F, T]) map2 [F[_], T] (x: T, y: T)(f: [t] => (F[t], t, t) => t): T =
      inst.erasedMap2(x, y)(f.asInstanceOf).asInstanceOf
    inline def (inst: ProductInstances[F, T]) foldLeft [F[_], T, Acc] (x: T)(i: Acc)(f: [t] => (Acc, F[t], t) => CompleteOr[Acc]): Acc =
      inst.erasedFoldLeft(x)(i)(f.asInstanceOf).asInstanceOf
    inline def (inst: ProductInstances[F, T]) foldLeft2 [F[_], T, Acc] (x: T, y: T)(i: Acc)(f: [t] => (Acc, F[t], t, t) => CompleteOr[Acc]): Acc =
      inst.erasedFoldLeft2(x, y)(i)(f.asInstanceOf).asInstanceOf

    inline def (inst: CoproductInstances[F, T]) project [F[_], T, Acc] (p: Int)(i: Acc)(f: [t] => (Acc, F[t]) => (Acc, Option[t])): (Acc, Option[T]) =
      inst.erasedProject(p)(i)(f.asInstanceOf).asInstanceOf
    inline def (inst: CoproductInstances[F, T]) fold [F[_], T, R] (x: T)(f: [t] => (F[t], t) => R): R =
      inst.erasedFold(x)(f.asInstanceOf).asInstanceOf
    inline def (inst: CoproductInstances[F, T]) fold2 [F[_], T, R] (x: T, y: T)(a: => R)(f: [t] => (F[t], t, t) => R): R =
      inst.erasedFold2(x, y)(a.asInstanceOf)(f.asInstanceOf).asInstanceOf
  }

  type ProductGenericR[O, R] = Mirror.Product { type MirroredType = O ; type MirroredElemTypes = R }
  type CoproductGenericR[O, R] = Mirror.Sum { type MirroredType = O ; type MirroredElemTypes = R }

  inline given mkInstances[F[_], T] as ErasedInstances[F[T]] given (gen: Generic[T]) =
    inline gen match {
      case p: ProductGeneric[T]   => mkProductInstances[F, T] given p
      case c: CoproductGeneric[T] => mkCoproductInstances[F, T] given c
    }

  inline given mkProductInstances[F[_], T] as ErasedProductInstances[F[T]] given (gen: ProductGeneric[T]) =
    ErasedProductInstances[F[T], LiftP[F, gen.MirroredElemTypes]](gen)

  inline given mkCoproductInstances[F[_], T] as ErasedCoproductInstances[F[T]] given (gen: CoproductGeneric[T]) =
    ErasedCoproductInstances[F[T], LiftP[F, gen.MirroredElemTypes]](gen)
}

object K1 {
  type Generic[O[_]] = Mirror { type MirroredType = O ; type MirroredElemTypes[_] }
  type ProductGeneric[O[_]] = Mirror.Product { type MirroredType = O ; type MirroredElemTypes[_] }
  type CoproductGeneric[O[_]] = Mirror.Sum { type MirroredType = O ; type MirroredElemTypes[_] }

  def Generic[O[_]] given (gen: Generic[O]): gen.type = gen
  def ProductGeneric[O[_]] given (gen: ProductGeneric[O]): gen.type = gen
  def CoproductGeneric[O[_]] given (gen: CoproductGeneric[O]): gen.type = gen

  type Instances[F[_[_]], T[_]] = ErasedInstances[F[T]]
  type ProductInstances[F[_[_]], T[_]] = ErasedProductInstances[F[T]]
  type CoproductInstances[F[_[_]], T[_]] = ErasedCoproductInstances[F[T]]

  def Instances[F[_[_]], T[_]] given (inst: Instances[F, T]): inst.type = inst
  def ProductInstances[F[_[_]], T[_]] given (inst: ProductInstances[F, T]): inst.type = inst
  def CoproductInstances[F[_[_]], T[_]] given (inst: CoproductInstances[F, T]): inst.type = inst

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
    case _: (a *: b) => implicit match {
      case aa: `a` => aa
      case _ => summonFirst0[b]
    }
  }

  given Ops {
    inline def (gen: ProductGeneric[Obj]) toRepr [Obj[_], A] (o: Obj[A]): gen.MirroredElemTypes[A] = Tuple.fromProduct(o.asInstanceOf).asInstanceOf[gen.MirroredElemTypes[A]]
    inline def (gen: ProductGeneric[Obj]) fromRepr [Obj[_], A] (r: gen.MirroredElemTypes[A]): Obj[A] = gen.fromProduct(r.asInstanceOf).asInstanceOf[Obj[A]]

    inline def (gen: CoproductGeneric[Obj]) toRepr [Obj[_], A] (o: Obj[A]): K0.ToUnion[gen.MirroredElemTypes[A]] = o.asInstanceOf
    inline def (gen: CoproductGeneric[Obj]) fromRepr [Obj[_], A] (r: K0.ToUnion[gen.MirroredElemTypes[A]]): Obj[A] = r.asInstanceOf

    inline def (inst: Instances[F, T]) map[F[_[_]], T[_], A, R](x: T[A])(f: [t[_]] => (F[t], t[A]) => t[R]): T[R] =
      inst.erasedMap(x)(f.asInstanceOf).asInstanceOf

    inline def (inst: ProductInstances[F, T]) construct [F[_[_]], T[_], R] (f: [t[_]] => F[t] => t[R]): T[R] =
      inst.erasedConstruct(f.asInstanceOf).asInstanceOf
    inline def (inst: ProductInstances[F, T]) map2 [F[_[_]], T[_], A, B, R] (x: T[A], y: T[B])(f: [t[_]] => (F[t], t[A], t[B]) => t[R]): T[R] =
      inst.erasedMap2(x, y)(f.asInstanceOf).asInstanceOf
    inline def (inst: ProductInstances[F, T]) foldLeft [F[_[_]], T[_], A, Acc] (x: T[A])(i: Acc)(f: [t[_]] => (Acc, F[t], t[A]) => CompleteOr[Acc]): Acc =
      inst.erasedFoldLeft(x)(i)(f.asInstanceOf).asInstanceOf
    inline def (inst: ProductInstances[F, T]) foldLeft2 [F[_[_]], T[_], A, B, Acc] (x: T[A], y: T[B])(i: Acc)(f: [t[_]] => (Acc, F[t], t[A], t[B]) => CompleteOr[Acc]): Acc =
      inst.erasedFoldLeft2(x, y)(i)(f.asInstanceOf).asInstanceOf

    inline def (inst: CoproductInstances[F, T]) fold [F[_[_]], T[_], A, R] (x: T[A])(f: [t[_]] => (F[t], t[A]) => R): R =
      inst.erasedFold(x)(f.asInstanceOf).asInstanceOf
    inline def (inst: CoproductInstances[F, T]) fold2 [F[_[_]], T[_], A, B, R] (x: T[A], y: T[B])(a: => R)(f: [t[_]] => (F[t], t[A], t[B]) => R): R =
      inst.erasedFold2(x, y)(a.asInstanceOf)(f.asInstanceOf).asInstanceOf
  }

  inline given mkInstances[F[_[_]], T[_]] as ErasedInstances[F[T]] given (gen: Generic[T]) =
    inline gen match {
      case p: ProductGeneric[T] => mkProductInstances[F, T] given p
      case c: CoproductGeneric[T] => mkCoproductInstances[F, T] given c
    }

  inline given mkProductInstances[F[_[_]], T[_]] as ErasedProductInstances[F[T]] given (gen: ProductGeneric[T]) =
    ErasedProductInstances[F[T], LiftP[F, gen.MirroredElemTypes]](gen)

  inline given mkCoproductInstances[F[_[_]], T[_]] as ErasedCoproductInstances[F[T]] given (gen: CoproductGeneric[T]) =
    ErasedCoproductInstances[F[T], LiftP[F, gen.MirroredElemTypes]](gen)

  type LiftProductGeneric[O, E] = ProductGeneric[Const[O]] { type MirroredElemTypes = Const[E] }

  given mkK1_0[O] as LiftProductGeneric[O, k0.MirroredElemTypes] given (k0: K0.ProductGeneric[O]) = k0.asInstanceOf
}

object K11 {
  type Generic[O[_[_]]] = Mirror { type MirroredType = O ; type MirroredElemTypes[_[_]] }
  type ProductGeneric[O[_[_]]] = Mirror.Product { type MirroredType = O ; type MirroredElemTypes[_[_]] }
  type CoproductGeneric[O[_[_]]] = Mirror.Sum { type MirroredType = O ; type MirroredElemTypes[_[_]] }

  def Generic[O[_[_]]] given (gen: Generic[O]): gen.type = gen
  def ProductGeneric[O[_[_]]] given (gen: ProductGeneric[O]): gen.type = gen
  def CoproductGeneric[O[_[_]]] given (gen: CoproductGeneric[O]): gen.type = gen

  type Instances[F[_[_[_]]], T[_[_]]] = ErasedInstances[F[T]]
  type ProductInstances[F[_[_[_]]], T[_[_]]] = ErasedProductInstances[F[T]]
  type CoproductInstances[F[_[_[_]]], T[_[_]]] = ErasedCoproductInstances[F[T]]

  def Instances[F[_[_[_]]], T[_[_]]] given (inst: Instances[F, T]): inst.type = inst
  def ProductInstances[F[_[_[_]]], T[_[_]]] given (inst: ProductInstances[F, T]): inst.type = inst
  def CoproductInstances[F[_[_[_]]], T[_[_]]] given (inst: CoproductInstances[F, T]): inst.type = inst

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

  given Ops {
    inline def (inst: Instances[F, T]) map[F[_[_[_]]], T[_[_]], A[_], R[_]](x: T[A])(f: [t[_[_]]] => (F[t], t[A]) => t[R]): T[R] =
      inst.erasedMap(x)(f.asInstanceOf).asInstanceOf

    inline def (inst: ProductInstances[F, T]) construct [F[_[_[_]]], T[_[_]], R[_]] (f: [t[_[_]]] => F[t] => t[R]): T[R] =
      inst.erasedConstruct(f.asInstanceOf).asInstanceOf
    inline def (inst: ProductInstances[F, T]) map2 [F[_[_[_]]], T[_[_]], A[_], B[_], R[_]] (x: T[A], y: T[B])(f: [t[_[_]]] => (F[t], t[A], t[B]) => t[R]): T[R] =
      inst.erasedMap2(x, y)(f.asInstanceOf).asInstanceOf
    inline def (inst: ProductInstances[F, T]) foldLeft [F[_[_[_]]], T[_[_]], A[_], Acc] (x: T[A])(i: Acc)(f: [t[_[_]]] => (Acc, F[t], t[A]) => CompleteOr[Acc]): Acc =
      inst.erasedFoldLeft(x)(i)(f.asInstanceOf).asInstanceOf
    inline def (inst: ProductInstances[F, T]) foldLeft2 [F[_[_[_]]], T[_[_]], A[_], B[_], Acc] (x: T[A], y: T[B])(i: Acc)(f: [t[_[_]]] => (Acc, F[t], t[A], t[B]) => CompleteOr[Acc]): Acc =
      inst.erasedFoldLeft2(x, y)(i)(f.asInstanceOf).asInstanceOf

    inline def (inst: CoproductInstances[F, T]) fold [F[_[_[_]]], T[_[_]], A[_], R] (x: T[A])(f: [t[_[_]]] => (F[t], t[A]) => R): R =
      inst.erasedFold(x)(f.asInstanceOf).asInstanceOf
    inline def (inst: CoproductInstances[F, T]) fold2 [F[_[_[_]]], T[_[_]], A[_], B[_], R] (x: T[A], y: T[B])(a: => R)(f: [t[_[_]]] => (F[t], t[A], t[B]) => R): R =
      inst.erasedFold2(x, y)(a.asInstanceOf)(f.asInstanceOf).asInstanceOf
  }

  inline given mkInstances[F[_[_[_]]], T[_[_]]] as ErasedInstances[F[T]] given (gen: Generic[T]) =
    inline gen match {
      case p: ProductGeneric[T]   => mkProductInstances[F, T] given p
      case c: CoproductGeneric[T] => mkCoproductInstances[F, T] given c
    }

  inline given mkProductInstances[F[_[_[_]]], T[_[_]]] as ErasedProductInstances[F[T]] given (gen: ProductGeneric[T]) =
    ErasedProductInstances[F[T], LiftP[F, gen.MirroredElemTypes]](gen)

  inline given mkCoproductInstances[F[_[_[_]]], T[_[_]]] as ErasedCoproductInstances[F[T]] given (gen: CoproductGeneric[T]) =
    ErasedCoproductInstances[F[T], LiftP[F, gen.MirroredElemTypes]](gen)
}

object K2 {
  type Generic[O[_, _]] = Mirror { type MirroredType = O ; type MirroredElemTypes[_, _] }
  type ProductGeneric[O[_, _]] = Mirror.Product { type MirroredType = O ; type MirroredElemTypes[_, _] }
  type CoproductGeneric[O[_, _]] = Mirror.Sum { type MirroredType = O ; type MirroredElemTypes[_, _] }

  def Generic[O[_, _]] given (gen: Generic[O]): gen.type = gen
  def ProductGeneric[O[_, _]] given (gen: ProductGeneric[O]): gen.type = gen
  def CoproductGeneric[O[_, _]] given (gen: CoproductGeneric[O]): gen.type = gen

  type Instances[F[_[_, _]], T[_, _]] = ErasedInstances[F[T]]
  type ProductInstances[F[_[_, _]], T[_, _]] = ErasedProductInstances[F[T]]
  type CoproductInstances[F[_[_, _]], T[_, _]] = ErasedCoproductInstances[F[T]]

  def Instances[F[_[_, _]], T[_, _]] given (inst: Instances[F, T]): inst.type = inst
  def ProductInstances[F[_[_, _]], T[_, _]] given (inst: ProductInstances[F, T]): inst.type = inst
  def CoproductInstances[F[_[_, _]], T[_, _]] given (inst: CoproductInstances[F, T]): inst.type = inst

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

  given Ops {
    inline def (inst: Instances[F, T]) map[F[_[_, _]], T[_, _], A, B, R, S](x: T[A, B])(f: [t[_, _]] => (F[t], t[A, B]) => t[R, S]): T[R, S] =
      inst.erasedMap(x)(f.asInstanceOf).asInstanceOf

    inline def (inst: ProductInstances[F, T]) construct [F[_[_, _]], T[_, _], R, S] (f: [t[_, _]] => F[t] => t[R, S]): T[R, S] =
      inst.erasedConstruct(f.asInstanceOf).asInstanceOf
    inline def (inst: ProductInstances[F, T]) map2 [F[_[_, _]], T[_, _], A, B, C, D, R, S] (x: T[A, B], y: T[C, D])(f: [t[_, _]] => (F[t], t[A, B], t[C, D]) => t[R, S]): T[R, S] =
      inst.erasedMap2(x, y)(f.asInstanceOf).asInstanceOf
    inline def (inst: ProductInstances[F, T]) foldLeft [F[_[_, _]], T[_, _], A, B, Acc] (x: T[A, B])(i: Acc)(f: [t[_, _]] => (Acc, F[t], t[A, B]) => CompleteOr[Acc]): Acc =
      inst.erasedFoldLeft(x)(i)(f.asInstanceOf).asInstanceOf
    inline def (inst: ProductInstances[F, T]) foldLeft2 [F[_[_, _]], T[_, _], A, B, C, D, Acc] (x: T[A, B], y: T[C, D])(i: Acc)(f: [t[_, _]] => (Acc, F[t], t[A, B], t[C, D]) => CompleteOr[Acc]): Acc =
      inst.erasedFoldLeft2(x, y)(i)(f.asInstanceOf).asInstanceOf

    inline def (inst: CoproductInstances[F, T]) fold [F[_[_, _]], T[_, _], A, B, R] (x: T[A, B])(f: [t[_, _]] => (F[t], t[A, B]) => R): R =
      inst.erasedFold(x)(f.asInstanceOf).asInstanceOf
    inline def (inst: CoproductInstances[F, T]) fold2 [F[_[_, _]], T[_, _], A, B, C, D, R] (x: T[A, B], y: T[C, D])(a: => R)(f: [t[_, _]] => (F[t], t[A, B], t[C, D]) => R): R =
      inst.erasedFold2(x, y)(a.asInstanceOf)(f.asInstanceOf).asInstanceOf
  }

  inline given mkInstances[F[_[_, _]], T[_, _]] as ErasedInstances[F[T]] given (gen: Generic[T]) =
    inline gen match {
      case p: ProductGeneric[T]   => mkProductInstances[F, T] given p
      case c: CoproductGeneric[T] => mkCoproductInstances[F, T] given c
    }

  inline given mkProductInstances[F[_[_, _]], T[_, _]] as ErasedProductInstances[F[T]] given (gen: ProductGeneric[T]) =
    ErasedProductInstances[F[T], LiftP[F, gen.MirroredElemTypes]](gen)

  inline given mkCoproductInstances[F[_[_, _]], T[_, _]] as ErasedCoproductInstances[F[T]] given (gen: CoproductGeneric[T]) =
    ErasedCoproductInstances[F[T], LiftP[F, gen.MirroredElemTypes]](gen)
}

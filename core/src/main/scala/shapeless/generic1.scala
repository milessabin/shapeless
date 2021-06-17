/*
 * Copyright (c) 2015-18 Miles Sabin
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

trait Generic1[F[_], +FR[_[_]]] extends Serializable {
  type R[t]

  lazy val fr: FR[R] = mkFrr

  def to[T](ft: F[T]): R[T]
  def from[T](rt: R[T]): F[T]

  def mkFrr: FR[R]
}

object Generic1 extends Generic10 with Generic1ScalaCompat {
  type Aux[F[_], +FR[_[_]], R0[_]] = Generic1[F, FR] { type R[t] = R0[t] }

  def unsafeInstance[F[_], FR[_[_]], R0[_]](f: F[Any] => R0[Any], g: R0[Any] => F[Any])(implicit lazyFr: => FR[R0]): Aux[F, FR, R0] = {
    new Generic1[F, FR] {
      type R[t] = R0[t]
      def mkFrr: FR[R] = lazyFr
      def to[T](ft: F[T]): R[T] = f(ft.asInstanceOf[F[Any]]).asInstanceOf[R[T]]
      def from[T](rt: R[T]): F[T] = g(rt.asInstanceOf[R[Any]]).asInstanceOf[F[T]]
    }
  }
}

trait Generic10 extends Generic10ScalaCompat

trait IsHCons1[L[_], +FH[_[_]], +FT[_[_]]] extends Serializable {
  type H[_]
  type T[_] <: HList

  lazy val fh: FH[H] = mkFhh
  lazy val ft: FT[T] = mkFtt

  def pack[A](u: (H[A], T[A])): L[A]
  def unpack[A](p: L[A]): (H[A], T[A])

  def mkFhh: FH[H]
  def mkFtt: FT[T]
}

object IsHCons1 extends IsHCons10 with IsHCons1ScalaCompat {
  type Aux[L[_], +FH[_[_]], +FT[_[_]], H0[_], T0[_] <: HList] = IsHCons1[L, FH, FT] { type H[t] = H0[t] ; type T[t] = T0[t] }

  def unsafeInstance[L[_] <: HList, FH[_[_]], FT[_[_]], H0[_], T0[_] <: HList](
    f: (H0[Any], T0[Any]) => L[Any],
    g: L[Any] => (H0[Any], T0[Any])
  )(implicit lazyFhh: => FH[H0], lazyFtt: => FT[T0]): Aux[L, FH, FT, H0, T0] =
    new IsHCons1[L, FH, FT] {
      type H[x] = H0[x]
      type T[x] = T0[x]
      def mkFhh: FH[H] = lazyFhh
      def mkFtt: FT[T] = lazyFtt
      def pack[A](u: (H[A], T[A])): L[A] =
        f(u._1.asInstanceOf[H[Any]], u._2.asInstanceOf[T[Any]]).asInstanceOf[L[A]]
      def unpack[A](p: L[A]): (H[A], T[A]) =
        g(p.asInstanceOf[L[Any]]).asInstanceOf[(H[A], T[A])]
    }
}

trait IsHCons10 extends IsHCons10ScalaCompat

trait IsCCons1[L[_], +FH[_[_]], +FT[_[_]]] extends Serializable {
  type H[_]
  type T[_] <: Coproduct

  lazy val fh: FH[H] = mkFhh
  lazy val ft: FT[T] = mkFtt

  def pack[A](u: Either[H[A], T[A]]): L[A]
  def unpack[A](p: L[A]): Either[H[A], T[A]]

  def mkFhh: FH[H]
  def mkFtt: FT[T]
}

object IsCCons1 extends IsCCons10 with IsCCons1ScalaCompat {
  type Aux[L[_], +FH[_[_]], +FT[_[_]], H0[_], T0[_] <: Coproduct] = IsCCons1[L, FH, FT] { type H[t] = H0[t] ; type T[t] = T0[t] }

  def unsafeInstance[L[_] <: Coproduct, FH[_[_]], FT[_[_]], H0[_], T0[_] <: Coproduct](
    f: Either[H0[Any], T0[Any]] => L[Any],
    g: L[Any] => Either[H0[Any], T0[Any]]
  )(implicit lazyFhh: => FH[H0], lazyFtt: => FT[T0]): Aux[L, FH, FT, H0, T0] =
    new IsCCons1[L, FH, FT] {
      type H[x] = H0[x]
      type T[x] = T0[x]
      def mkFhh: FH[H] = lazyFhh
      def mkFtt: FT[T] = lazyFtt
      def pack[A](u: Either[H[A], T[A]]): L[A] =
        f(u.asInstanceOf[Either[H[Any], T[Any]]]).asInstanceOf[L[A]]
      def unpack[A](p: L[A]): Either[H[A], T[A]] =
        g(p.asInstanceOf[L[Any]]).asInstanceOf[Either[H[A], T[A]]]
    }
}

trait IsCCons10 extends IsCCons10ScalaCompat

trait Split1[L[_], +FO[_[_]], +FI[_[_]]] extends Serializable {
  type O[_]
  type I[_]

  lazy val fo: FO[O] = mkFoo
  lazy val fi: FI[I] = mkFii

  def pack[T](u: O[I[T]]): L[T]
  def unpack[T](p: L[T]): O[I[T]]

  def mkFoo: FO[O]
  def mkFii: FI[I]
}

object Split1 extends Split10 with Split1ScalaCompat {
  type Aux[L[_], +FO[_[_]], +FI[_[_]], O0[_], I0[_]] = Split1[L, FO, FI] { type O[T] = O0[T] ; type I[T] = I0[T] }

  def instance[FO[_[_]], FI[_[_]], O0[_], I0[_]](implicit lazyFoo: => FO[O0], lazyFii: => FI[I0]): Aux[({ type λ[x] = O0[I0[x]] })#λ, FO, FI, O0, I0] =
    new Split1[({ type λ[x] = O0[I0[x]] })#λ, FO, FI] {
      type O[x] = O0[x]
      type I[x] = I0[x]
      def mkFoo: FO[O] = lazyFoo
      def mkFii: FI[I] = lazyFii
      def pack[T](u: O[I[T]]): O[I[T]] = u
      def unpack[T](p: O[I[T]]): O[I[T]] = p
    }
}

trait Split10 extends Split10ScalaCompat

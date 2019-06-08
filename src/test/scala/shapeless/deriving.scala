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

import org.junit.Test

import scala.compiletime._

// Type classes

trait Monoid[A] {
  def empty: A
  def combine(x: A, y: A): A
}

object Monoid {
  inline def apply[A](implicit ma: Monoid[A]): Monoid[A] = ma

  implicit val monoidUnit: Monoid[Unit] = new Monoid[Unit] {
    def empty: Unit = ()
    def combine(x: Unit, y: Unit): Unit = ()
  }
  implicit val monoidBoolean: Monoid[Boolean] = new Monoid[Boolean] {
    def empty: Boolean = false
    def combine(x: Boolean, y: Boolean): Boolean = x || y
  }
  implicit val monoidInt: Monoid[Int] = new Monoid[Int] {
    def empty: Int = 0
    def combine(x: Int, y: Int): Int = x+y
  }
  implicit val monoidString: Monoid[String] = new Monoid[String] {
    def empty: String = ""
    def combine(x: String, y: String): String = x+y
  }

  implicit def monoidGen[A](implicit inst: K0.ProductInstances[Monoid, A]): Monoid[A] =
    new Monoid[A] {
      def empty: A = inst.construct([t] => (ma: Monoid[t]) => ma.empty)
      def combine(x: A, y: A): A = inst.map2(x, y)([t] => (mt: Monoid[t], t0: t, t1: t) => mt.combine(t0, t1))
    }

  inline def derived[A](implicit gen: K0.ProductGeneric[A]): Monoid[A] =
    monoidGen(K0.mkProductInstances[Monoid, A](gen))
}

trait Eq[A] {
  def eqv(x: A, y: A): Boolean
}

object Eq {
  inline def apply[A](implicit ea: Eq[A]): Eq[A] = ea

  implicit val eqUnit: Eq[Unit] = new Eq[Unit] {
    def eqv(x: Unit, y: Unit): Boolean = true
  }
  implicit val eqBoolean: Eq[Boolean] = new Eq[Boolean] {
    def eqv(x: Boolean, y: Boolean): Boolean = x == y
  }
  implicit val eqInt: Eq[Int] = new Eq[Int] {
    def eqv(x: Int, y: Int): Boolean = x == y
  }
  implicit val eqString: Eq[String] = new Eq[String] {
    def eqv(x: String, y: String): Boolean = x == y
  }

  implicit def eqGen[A](implicit inst: => K0.ProductInstances[Eq, A]): Eq[A] =
    new Eq[A] {
      def eqv(x: A, y: A): Boolean = inst.foldLeft2(x, y)(true: Boolean)(
        [t] => (acc: Boolean, eqt: Eq[t], t0: t, t1: t) => Complete(!eqt.eqv(t0, t1))(false)(true)
      )
    }

  implicit def eqGenC[A](implicit inst: => K0.CoproductInstances[Eq, A]): Eq[A] =
    new Eq[A] {
      def eqv(x: A, y: A): Boolean = inst.fold2(x, y)(false)(
        [t] => (eqt: Eq[t], t0: t, t1: t) => eqt.eqv(t0, t1)
      )
    }

  inline def derived[A](implicit gen: K0.Generic[A]): Eq[A] = K0.derive(gen, eqGen, eqGenC)
}

trait Functor[F[_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B]
}

object Functor {
  inline def apply[F[_]](implicit ff: Functor[F]): Functor[F] = ff

  implicit val functorId: Functor[Id] = new Functor[Id] {
    def map[A, B](a: A)(f: A => B): B = f(a)
  }

  implicit def functorNested[F[_], G[_]](implicit ff: Functor[F], fg: Functor[G]): Functor[[t] =>> F[G[t]]] =
    new Functor[[t] =>> F[G[t]]] {
      def map[A, B](fga: F[G[A]])(f: A => B): F[G[B]] = ff.map(fga)(ga => fg.map(ga)(f))
    }

  implicit def functorGen[F[_]](implicit inst: => K1.Instances[Functor, F]): Functor[F] =
    new Functor[F] {
      def map[A, B](fa: F[A])(f: A => B): F[B] = inst.map(fa)([t[_]] => (ft: Functor[t], ta: t[A]) => ft.map(ta)(f))
    }

  implicit def functorConst[T]: Functor[Const[T]] = new Functor[Const[T]] {
    def map[A, B](t: T)(f: A => B): T = t
  }

  inline def derived[F[_]](implicit gen: K1.Generic[F]): Functor[F] =
    functorGen(K1.mkInstances[Functor, F](gen))
}

trait FunctorK[H[_[_]]] {
  def mapK[A[_], B[_]](af: H[A])(f: A ~> B): H[B]
}

object FunctorK {
  inline def apply[H[_[_]]](implicit fh: FunctorK[H]): FunctorK[H] = fh

  implicit def functorKApplyTo[T]: FunctorK[K11.Id[T]] =
    new FunctorK[K11.Id[T]] {
      def mapK[A[_], B[_]](at: A[T])(f: A ~> B): B[T] = f(at)
    }


  implicit def functorKGen[H[_[_]]](implicit inst: => K11.Instances[FunctorK, H]): FunctorK[H] =
    new FunctorK[H] {
      def mapK[A[_], B[_]](ha: H[A])(f: A ~> B): H[B] =
        inst.map(ha)([t[_[_]]] => (ft: FunctorK[t], ta: t[A]) => ft.mapK(ta)(f))
    }

  implicit def functorKConst11[T]: FunctorK[K11.Const[T]] =
    new FunctorK[K11.Const[T]] {
      def mapK[A[_], B[_]](t: T)(f: A ~> B): T = t
    }

  inline def derived[F[_[_]]](implicit gen: K11.Generic[F]): FunctorK[F] =
    functorKGen(K11.mkInstances[FunctorK, F](gen))
}

case class Fix[S[_, _], A](unfix: S[A, Fix[S, A]])

trait Bifunctor[F[_, _]] {
  def bimap[A, B, C, D](fab: F[A, B])(f: A => C, g: B => D): F[C, D]
}

object Bifunctor {
  inline def apply[F[_, _]](implicit bf: Bifunctor[F]): Bifunctor[F] = bf

  def map[S[_, _], A, B](f: A => B)(fsa: Fix[S, A])(implicit bs: Bifunctor[S]): Fix[S, B] =
    Fix(bs.bimap(fsa.unfix)(f, map(f)))

  implicit def bifunctorPair: Bifunctor[Tuple2] =
    new Bifunctor[Tuple2] {
      def bimap[A, B, C, D](fab: (A, B))(f: A => C, g: B => D): (C, D) =
        (f(fab._1), g(fab._2))
    }

  implicit def bifunctorEither: Bifunctor[Either] =
    new Bifunctor[Either] {
      def bimap[A, B, C, D](fab: Either[A, B])(f: A => C, g: B => D): Either[C, D] =
        fab match {
          case Left(a) => Left(f(a))
          case Right(b) => Right(g(b))
        }
    }

  implicit def bifunctorGen[F[_, _]](implicit inst: => K2.Instances[Bifunctor, F]): Bifunctor[F] =
    new Bifunctor[F] {
      def bimap[A, B, C, D](fab: F[A, B])(f: A => C, g: B => D): F[C, D] =
        inst.map(fab)([t[_, _]] => (bft: Bifunctor[t], tab: t[A, B]) => bft.bimap(tab)(f, g))
    }

  implicit def bifunctorFirst: Bifunctor[K2.Id1] = new Bifunctor[K2.Id1] {
    def bimap[A, B, C, D](a: A)(f: A => C, g: B => D): C = f(a)
  }

  implicit def bifunctorSecond: Bifunctor[K2.Id2] = new Bifunctor[K2.Id2] {
    def bimap[A, B, C, D](b: B)(f: A => C, g: B => D): D = g(b)
  }

  implicit def bifunctorConst[T]: Bifunctor[K2.Const[T]] = new Bifunctor[K2.Const[T]] {
    def bimap[A, B, C, D](t: T)(f: A => C, g: B => D): T = t
  }

  inline def derived[F[_, _]](implicit gen: K2.Generic[F]): Bifunctor[F] =
    bifunctorGen(K2.mkInstances[Bifunctor, F](gen))
}

trait Case[F, A, B] extends (A => B)

trait Data[F, T, R] {
  def gmapQ(t: T): List[R]
}

object Data extends Data0 {
  def apply[F, T, R](implicit dt: Data[F, T, R]): Data[F, T, R] = dt

  type DFR[F, R] = [t] =>> Data[F, t, R]

  implicit def dataGen[F, T, R](implicit inst: => K0.ProductInstances[DFR[F, R], T]): Data[F, T, R] =
    mkData[F, T, R](t => inst.foldLeft[DFR[F, R], T, List[R]](t)(List.empty[R])(
      [t] => (acc: List[R], dt: Data[F, t, R], t: t) => Continue(dt.gmapQ(t) reverse_::: acc)
    ).reverse)

  implicit def dataGenC[F, T, R](implicit inst: => K0.CoproductInstances[DFR[F, R], T]): Data[F, T, R] =
    mkData[F, T, R](t => inst.fold[DFR[F, R], T, List[R]](t)(
      [t] => (dt: Data[F, t, R], t: t) => dt.gmapQ(t)
    ))

  inline def derived[F, T, R](implicit gen: K0.ProductGeneric[T]): Data[F, T, R] =
    dataGen(K0.mkProductInstances[DFR[F, R], T](gen))

  inline def derived[F, T, R](implicit gen: K0.CoproductGeneric[T]): Data[F, T, R] =
    dataGenC(K0.mkCoproductInstances[DFR[F, R], T](gen))
}

trait Data0 {
  def mkData[F, T, R](f: T => List[R]): Data[F, T, R] =
    new Data[F, T, R] {
      def gmapQ(t: T): List[R] = f(t)
    }

  inline implicit def dataDefault[F, T, R]: Data[F, T, R] = implicit match {
    case fn: Case[F, T, R] => mkData[F, T, R](t => List(fn(t)))
    case _ => mkData[F, T, R](_ => Nil)
  }
}

trait DataT[F, T] {
  type Out
  def gmapT(t: T): Out
}

object DataT {
  type Aux[F, T, Out0] = DataT[F, T] { type Out = Out0 }

  def apply[F, T](implicit dtt: DataT[F, T]): Aux[F, T, dtt.Out] = dtt

  type DF[F] = [t] =>> Aux[F, t, t]

  implicit def dataTGen[F, T](implicit inst: => K0.Instances[DF[F], T]): Aux[F, T, T] =
    mkDataT[F, T, T](t => inst.map[DF[F], T](t)(
      [t] => (dt: Aux[F, t, t], t: t) => dt.gmapT(t)
    ))

  def mkDataT[F, T, R](f: T => R): Aux[F, T, R] =
    new DataT[F, T] {
      type Out = R
      def gmapT(t: T): R = f(t)
    }

  inline implicit def dataTDefault[F, T, R]: Aux[F, T, R] = implicit match {
    case fn: Case[F, T, R] => mkDataT[F, T, R](fn)
    case ev: (T <:< R) => mkDataT[F, T, R](ev)
  }

  inline def derived[F, T](implicit gen: K0.Generic[T]): DataT[F, T] =
    dataTGen(K0.mkInstances[DF[F], T](gen))
}

trait Empty[T] {
  def empty: T
}

object Empty {
  def apply[T](implicit et: Empty[T]): Empty[T] = et

  def mkEmpty[T](t: T): Empty[T] =
    new Empty[T] {
      def empty = t
    }

  implicit def emptyUnit: Empty[Unit] = mkEmpty(())
  implicit def emptyInt: Empty[Int] = mkEmpty(0)
  implicit def emptyString: Empty[String] = mkEmpty("")
  implicit def emptyBoolean: Empty[Boolean] = mkEmpty(false)

  implicit def emptyGen[A](implicit inst: => K0.ProductInstances[Empty, A]): Empty[A] =
    mkEmpty(inst.construct([a] => (ma: Empty[a]) => ma.empty))

  inline implicit def emptyGenC[A](implicit gen: K0.CoproductGeneric[A]): Empty[A] =
    mkEmpty(K0.summonFirst[Empty, gen.MirroredElemTypes, A].empty)

  inline def derived[A](implicit gen: K0.Generic[A]): Empty[A] =
    inline gen match {
      case p: K0.ProductGeneric[A]   => emptyGen(K0.mkProductInstances[Empty, A](p))
      case c: K0.CoproductGeneric[A] => emptyGenC(c)
    }
}

trait EmptyK[F[_]] {
  def empty[A]: F[A]
}

object EmptyK {
  def apply[F[_]](implicit ef: EmptyK[F]): EmptyK[F] = ef

  def mkEmptyK[F[_]](f: [a] => () => F[a]): EmptyK[F] =
    new EmptyK[F] {
      def empty[A] = f[A]()
    }

  implicit def emptyKGen[A[_]](implicit inst: => K1.ProductInstances[EmptyK, A]): EmptyK[A] =
    mkEmptyK([t] => () => inst.construct([f[_]] => (ef: EmptyK[f]) => ef.empty[t]))

  inline implicit def emptyKGenC[A[_]](implicit gen: K1.CoproductGeneric[A]): EmptyK[A] =
    mkEmptyK[A]([t] => () => K1.summonFirst[EmptyK, gen.MirroredElemTypes, A].empty[t])

  inline def derived[A[_]](implicit gen: K1.Generic[A]): EmptyK[A] =
    inline gen match {
      case p: K1.ProductGeneric[A]   => emptyKGen(K1.mkProductInstances[EmptyK, A](p))
      case c: K1.CoproductGeneric[A] => emptyKGenC(c)
    }
}

trait Alt1[F[_[_]], G[_[_]], T[_]] {
  def fold[A](f: F[T] => A)(g: G[T] => A): A
}

object Alt1 {
  type Of[F[_[_]], G[_[_]]] = [t[_]] =>> Alt1[F, G, t]

  class Alt1F[F[_[_]], G[_[_]], T[_]](ft: F[T]) extends Alt1[F, G, T] {
    def fold[A](f: F[T] => A)(g: G[T] => A): A = f(ft)
  }

  class Alt1G[F[_[_]], G[_[_]], T[_]](gt: G[T]) extends Alt1[F, G, T] {
    def fold[A](f: F[T] => A)(g: G[T] => A): A = g(gt)
  }

  inline implicit def apply[F[_[_]], G[_[_]], T[_]]: Alt1[F, G, T] = implicit match {
    case ft: F[T] => new Alt1F(ft)
    case gt: G[T] => new Alt1G(gt)
  }
}

trait Pure[F[_]] {
  def pure[A](a: A): F[A]
}

object Pure {
  def apply[F[_]](implicit ef: Pure[F]): Pure[F] = ef

  def mkPure[F[_]](f: [a] => a => F[a]): Pure[F] =
    new Pure[F] {
      def pure[A](a: A) = f(a)
    }

  implicit def pureId: Pure[Id] = mkPure([T] => (t: T) => t)

  implicit def pureGen[A[_]](implicit inst: K1.ProductInstances[Alt1.Of[Pure, EmptyK], A]): Pure[A] =
    mkPure[A]([t] => (a: t) => inst.construct([f[_]] => (af: Alt1.Of[Pure, EmptyK][f]) => af.fold[f[t]](_.pure(a))(_.empty[t])))

  inline implicit def pureGenC[A[_]](implicit gen: K1.CoproductGeneric[A]): Pure[A] =
    mkPure[A]([t] => (a: t) => K1.summonFirst[Pure, gen.MirroredElemTypes, A].pure(a))

  inline def derived[A[_]](implicit gen: K1.Generic[A]): Pure[A] =
    inline gen match {
      case p: K1.ProductGeneric[A]   => pureGen(K1.mkProductInstances[Alt1.Of[Pure, EmptyK], A](p))
      case c: K1.CoproductGeneric[A] => pureGenC(c)
    }
}

trait Show[T] {
  def show(t: T): String
}

object Show {
  inline def apply[T](implicit st: Show[T]): Show[T] = st

  def mkShow[T](f: T => String): Show[T] =
    new Show[T] {
      def show(t: T): String = f(t)
    }

  implicit val showInt: Show[Int] = (_: Int).toString
  implicit val showString: Show[String] = (s: String) => "\""+s+"\""
  implicit val showBoolean: Show[Boolean] = (_: Boolean).toString

  implicit def showGen[T](implicit inst: => K0.ProductInstances[Show, T], labelling: Labelling[T]): Show[T] =
    new Show[T] {
      def show(t: T): String = {
        if(labelling.elemLabels.isEmpty) labelling.label
        else {
          val elems: List[String] = inst.foldLeft(t)(List.empty[String])(
            [t] => (acc: List[String], st: Show[t], t: t) => Continue(st.show(t) :: acc)
          )
          labelling.elemLabels.zip(elems.reverse).map((k, v) => s"$k: $v").mkString(s"${labelling.label}(", ", ", ")")
        }
      }
    }

  implicit def showGenC[T](implicit inst: => K0.CoproductInstances[Show, T]): Show[T] =
    new Show[T] {
      def show(t: T): String = inst.fold(t)([t] => (st: Show[t], t: t) => st.show(t))
    }

  inline def derived[A](implicit gen: K0.Generic[A]): Show[A] =
    K0.derive(gen, inst => showGen given (inst, Labelling(gen)), showGenC)
}

trait Read[T] {
  def read(s: String): Option[(T, String)]
}

object Read {
  inline def apply[T](implicit rt: Read[T]): Read[T] = rt

  import scala.util.matching.Regex
  import scala.util.Try

  def head(s: String, r: Regex): Option[(String, String)] =
    s.trim match {
      case r(hd, tl) => Some((hd, tl))
      case _ => None
    }

  def readPrimitive[T](r: Regex, f: String => Option[T]): Read[T] =
    (s: String) =>
      for {
        (hd, tl) <- head(s, r)
        p        <- f(hd)
      } yield (p, tl)


  implicit val readInt: Read[Int] = readPrimitive("""(-?\d*)(.*)""".r, s => Try(s.toInt).toOption)
  implicit val readString: Read[String] = (s: String) => head(s, """\"(.*)\"(.*)""".r)
  implicit val readBoolean: Read[Boolean] = readPrimitive("""(true|false)(.*)""".r, s => Try(s.toBoolean).toOption)

  implicit def readGen[T](implicit inst: => K0.ProductInstances[Read, T], labelling: Labelling[T]): Read[T] =
    new Read[T] {
      def read(s: String): Option[(T, String)] = {
        def readUnit(s: String): Option[(T, String)] = {
          inst.unfold[Read, T, Unit](())(
            [t] => (u: Unit, rt: Read[t]) => ((), None)
          )._2.map(t => (t, s))
        }

        def readElems(s: String): Option[(T, String)] = {
          type Acc = (String, Seq[String], Boolean)
          inst.unfold[Read, T, Acc]((s, labelling.elemLabels, true))(
            [t] => (acc: Acc, rt: Read[t]) => {
              val (s, labels, first) = acc
              (for {
                (_, tl0) <- if(first) Some(("", s)) else head(s, "(,)(.*)".r)
                (_, tl1) <- head(tl0, s"(${labels.head}):(.*)".r)
                (t, tl2) <- rt.read(tl1)
                } yield (t, tl2)) match {
                  case Some(t, tl2) => ((tl2, labels.tail, false), Some(t))
                  case None => ((s, labels, first), None)
                }
            }
            ) match {
              case (s, None) => None
              case (acc, Some(t)) => Some((t, acc._1))
            }
        }

        if(labelling.elemLabels.isEmpty) {
          for {
            (_, tl0) <- head(s, s"(${labelling.label})(.*)".r)
            (t, tl1) <- readUnit(tl0)
          } yield (t, tl1)
        } else {
          for {
            (_, tl0) <- head(s, s"(${labelling.label})\\((.*)".r)
            (t, tl1) <- readElems(tl0)
            (_, tl2) <- head(tl1, s"(\\))(.*)".r)
          } yield (t, tl2)
        }
      }
    }

  implicit def readGenC[T](implicit inst: => K0.CoproductInstances[Read, T], labelling: Labelling[T]): Read[T] =
    new Read[T] {
      def read(s: String): Option[(T, String)] = {
        labelling.elemLabels.zipWithIndex.iterator.map((p: (String, Int)) => {
          val (label, i) = p
          if(s.trim.startsWith(label)) {
            inst.project[Read, T, String](i)(s)(
              [t] => (s: String, rt: Read[t]) =>
                rt.read(s) match {
                  case Some((t, tl)) => (tl, Some(t))
                  case None => (s, None)
                }
            ) match {
              case (s, None) => None
              case (tl, Some(t)) => Some((t, tl))
            }
          }
          else None
        }).find(_.isDefined).flatten
      }
    }

  inline def derived[A](implicit gen: K0.Generic[A]): Read[A] =
    K0.derive(gen, inst => readGen given (inst, Labelling(gen)), inst => readGenC given (inst, Labelling(gen)))
}

trait Transform[T, U] {
  def apply(t: T): U
}

object Transform {
  def apply[T, U](implicit ttu: Transform[T, U]): Transform[T, U] = ttu

  inline def mkField[K, KT, RT <: NonEmptyTuple, V](rt: RT): Object =
    (inline constValue[K0.IndexOf[K, KT]] match {
      case -1 => summon[Monoid[V]].empty
      case i => rt(i)
    }).asInstanceOf

  inline def mkFieldArray[KU, RU, KT, RT <: NonEmptyTuple](rt: RT): Array[Object] =
    inline erasedValue[(KU, RU)] match {
      case _: (Unit, Unit) => Array()
      case _: (Tuple1[k0], Tuple1[v0]) =>
        Array(
          mkField[k0, KT, RT, v0](rt)
        )
      case _: ((k0, k1), (v0, v1)) =>
        Array(
          mkField[k0, KT, RT, v0](rt),
          mkField[k1, KT, RT, v1](rt)
        )
      case _: ((k0, k1, k2), (v0, v1, v2)) =>
        Array(
          mkField[k0, KT, RT, v0](rt),
          mkField[k1, KT, RT, v1](rt),
          mkField[k2, KT, RT, v2](rt)
        )

      // Add fallback for larger sizes
    }

  inline def mkRecord[KU, RU <: Tuple, KT, RT <: NonEmptyTuple](rt: RT): RU =
    Tuple.fromArray(mkFieldArray[KU, RU, KT, RT](rt)).asInstanceOf

  inline implicit def transformGen[T, U]
    (implicit
      gent: K0.ProductGeneric[T] { type MirroredElemTypes <: NonEmptyTuple },
      genu: K0.ProductGeneric[U] { type MirroredElemTypes <: Tuple }
    ): Transform[T, U] =
      new Transform[T, U] {
        def apply(t: T): U =
          genu.fromRepr(mkRecord[genu.MirroredElemLabels, genu.MirroredElemTypes, gent.MirroredElemLabels, gent.MirroredElemTypes](gent.toRepr(t)))
      }
}

// ADTs

case class ISB(i: Int, s: String, b: Boolean) derives Monoid, Eq, Empty, Show, Read

case class Box[A](x: A) derives Monoid, Eq, Show, Read, Functor, Pure

sealed trait OptionInt derives Eq, Empty, Show, Read
case class SomeInt(value: Int) extends OptionInt
case object NoneInt extends OptionInt

sealed trait Opt[+A] derives Eq, Show, Read, Functor, EmptyK, Pure
case class Sm[+A](value: A) extends Opt[A]
case object Nn extends Opt[Nothing]

sealed trait CList[+A] derives Eq, Show, Read, Functor, EmptyK
case class CCons[+A](hd: A, tl: CList[A]) extends CList[A]
case object CNil extends CList[Nothing]

case class Order[F[_]](
  item: F[String],
  quantity: F[Int]
) derives FunctorK

sealed trait OptionD[T] {
  def fold: T = this match {
    case Given(t) => t
    case Default(t) => t
  }
}
object OptionD {
  val fold: OptionD ~> Id = [t] => (ot: OptionD[t]) => ot.fold
}

case class Given[T](value: T) extends OptionD[T]
case class Default[T](value: T) extends OptionD[T]

sealed trait ListF[+A, +R] derives Bifunctor
object ListF {
  type List[A] = Fix[ListF, A]
}
case class ConsF[+A, +R](hd: A, tl: R) extends ListF[A, R]
case object NilF extends ListF[Nothing, Nothing]

case class BI(b: Boolean, i: Int)

// Tests
object Size {
  implicit def intCase: Case[Size.type, Int, Int] = identity(_)
  implicit def stringCase: Case[Size.type, String, Int] = _.length
  implicit def booleanCase: Case[Size.type, Boolean, Int] = _ => 1
}

object Inc {
  implicit def intCase: Case[Inc.type, Int, Int] = _+1
  implicit def stringCase: Case[Inc.type, String, String] = _+"!"
  implicit def booleanCase: Case[Inc.type, Boolean, Boolean] = !_
}

class DerivationTests {
  @Test
  def monoid: Unit = {
    val v0 = Monoid[ISB]
    assert(v0.empty == ISB(0, "", false))
    assert(v0.combine(ISB(1, "foo", false), ISB(2, "bar", true)) == ISB(3, "foobar", true))

    val v1 = Monoid[Box[Int]]
    assert(v1.empty == Box(0))
    assert(v1.combine(Box(1), Box(2)) == Box(3))
  }

  @Test
  def eq: Unit = {
    val v0 = Eq[SomeInt]
    assert(v0.eqv(SomeInt(23), SomeInt(23)))
    assert(!v0.eqv(SomeInt(23), SomeInt(13)))
    val v1 = Eq[NoneInt.type]
    assert(v1.eqv(NoneInt, NoneInt))
    val v2 = Eq[OptionInt]
    assert(v2.eqv(SomeInt(23), SomeInt(23)))
    assert(!v2.eqv(SomeInt(23), SomeInt(13)))
    assert(!v2.eqv(SomeInt(23), NoneInt))

    val v3 = Eq[Sm[Int]]
    assert(v3.eqv(Sm(23), Sm(23)))
    assert(!v3.eqv(Sm(23), Sm(13)))
    val v4 = Eq[Nn.type]
    assert(v4.eqv(Nn, Nn))
    val v5 = Eq[Opt[Int]]
    assert(v5.eqv(Sm(23), Sm(23)))
    assert(!v5.eqv(Sm(23), Sm(13)))
    assert(!v5.eqv(Sm(23), Nn))

    val v6 = Eq[CNil.type]
    assert(v6.eqv(CNil, CNil))
    val v7 = Eq[CCons[Int]]
    assert(v7.eqv(CCons(1, CCons(2, CCons(3, CNil))), CCons(1, CCons(2, CCons(3, CNil)))))
    assert(!v7.eqv(CCons(1, CCons(2, CCons(3, CNil))), CCons(1, CCons(4, CCons(3, CNil)))))
    val v8 = Eq[CList[Int]]
    assert(v8.eqv(CCons(1, CCons(2, CCons(3, CNil))), CCons(1, CCons(2, CCons(3, CNil)))))
    assert(!v8.eqv(CCons(1, CCons(2, CCons(3, CNil))), CCons(1, CCons(4, CCons(3, CNil)))))
  }

  @Test
  def functor: Unit = {
    val v0 = Functor[Box]
    assert(v0.map(Box("foo"))(_.length) == Box(3))

    val v1 = Functor[Sm]
    assert(v1.map(Sm("foo"))(_.length) == Sm(3))
    val v2 = Functor[Const[Nn.type]]
    assert(v2.map(Nn)(identity) == Nn)
    val v3 = Functor[Opt]
    assert(v3.map(Sm("foo"))(_.length) == Sm(3))
    assert(v3.map(Nn)(identity) == Nn)

    val v4 = Functor[Const[CNil.type]]
    assert(v4.map(CNil)(identity) == CNil)
    val v5 = Functor[CCons]
    assert(v5.map(CCons("foo", CCons("quux", CCons("wibble", CNil))))(_.length) == CCons(3, CCons(4, CCons(6, CNil))))
    val v6 = Functor[CList]
    assert(v6.map(CCons("foo", CCons("quux", CCons("wibble", CNil))))(_.length) == CCons(3, CCons(4, CCons(6, CNil))))
    val v7 = Functor[[t] =>> CList[Opt[t]]]
    assert(v7.map(CCons(Sm("foo"), CCons(Nn, CCons(Sm("quux"), CNil))))(_.length) == CCons(Sm(3), CCons(Nn, CCons(Sm(4), CNil))))
  }

  @Test
  def functork: Unit = {
    val v0 = FunctorK[Order]
    assert(v0.mapK(Order[OptionD](Given("Epoisse"), Default(10)))(OptionD.fold) == Order[Id]("Epoisse", 10))
  }

  @Test
  def bifunctor: Unit = {
    val v0 = Bifunctor[ConsF]
    val v1 = Bifunctor[ListF]
    val v2: ListF.List[String] = Fix(ConsF("foo", Fix(ConsF("quux", Fix(ConsF("wibble", Fix(NilF)))))))
    val v3: ListF.List[Int] = Fix(ConsF(3, Fix(ConsF(4, Fix(ConsF(6, Fix(NilF)))))))
    assert(Bifunctor.map((_: String).length)(v2) == v3)
  }

  @Test
  def data: Unit = {
    val v0 = Data[Size.type, ISB, Int]
    assert(v0.gmapQ(ISB(23, "foo", true)).sum == 27)
    val v1 = Data[Size.type, OptionInt, Int]
    assert(v1.gmapQ(NoneInt).sum == 0)
    assert(v1.gmapQ(SomeInt(23)).sum == 23)
    val v2 = Data[Size.type, CList[String], Int]
    assert(v2.gmapQ(CCons("foo", CCons("quux", CCons("wibble", CNil)))).sum == 13)
  }

  @Test
  def datat: Unit = {
    val v0 = DataT[Inc.type, ISB]
    assert(v0.gmapT(ISB(23, "foo", true)) == ISB(24, "foo!", false))
    val v1 = DataT[Inc.type, OptionInt]
    assert(v1.gmapT(NoneInt) == NoneInt)
    assert(v1.gmapT(SomeInt(23)) == SomeInt(24))
    val v2 = DataT[Inc.type, CList[Int]]
    assert(v2.gmapT(CCons(1, CCons(2, CCons(3, CNil)))) == CCons(2, CCons(3, CCons(4, CNil))))
  }

  @Test
  def empty: Unit = {
    val v0 = Empty[ISB]
    assert(v0.empty == ISB(0, "", false))
  }

  @Test
  def emptyk: Unit = {
    val v0 = EmptyK[Opt]
    assert(v0.empty[Int] == Nn)
    val v1 = EmptyK[CList]
    assert(v1.empty[Int] == CNil)
  }

  @Test
  def pure: Unit = {
    val v0 = Pure[Box]
    assert(v0.pure(23) == Box(23))
    val v1 = Pure[CList]
    assert(v1.pure(23) == CCons(23, CNil))
  }

  @Test
  def labels: Unit = {
    val v0 = K0.Generic[ISB]
    val v1 = summonValues[v0.MirroredElemLabels]
    assert(v1 == ("i", "s", "b"))
  }

  @Test
  def show: Unit = {
    val v0 = Show[ISB]
    assert(v0.show(ISB(23, "foo", true)) == """ISB(i: 23, s: "foo", b: true)""")

    val v1 = Show[OptionInt]
    assert(v1.show(SomeInt(23)) == "SomeInt(value: 23)")
    assert(v1.show(NoneInt) == "NoneInt")

    val v2 = Show[Box[Int]]
    assert(v2.show(Box(23)) == "Box(x: 23)")

    val v3 = Show[Opt[Int]]
    assert(v3.show(Sm(23)) == "Sm(value: 23)")
    assert(v3.show(Nn) == "Nn")

    val v4 = Show[CList[Int]]
    assert(v4.show((CCons(1, CCons(2, CCons(3, CNil))))) == "CCons(hd: 1, tl: CCons(hd: 2, tl: CCons(hd: 3, tl: CNil)))")

    val v5 = Show[Order[Id]]
    assert(v5.show(Order[Id]("Epoisse", 10)) == """Order(item: "Epoisse", quantity: 10)""")
  }

  @Test
  def read: Unit = {
    val v0 = Read[ISB]
    assert(v0.read("""ISB(i: 23, s: "foo", b: true)""") == Some((ISB(23, "foo", true), "")))

    val v1 = Read[OptionInt]
    assert(v1.read("SomeInt(value: 23)") == Some((SomeInt(23), "")))
    assert(v1.read("NoneInt") == Some((NoneInt, "")))

    val v2 = Read[Box[Int]]
    assert(v2.read("Box(x: 23)") == Some((Box(23), "")))

    val v3 = Read[Opt[Int]]
    assert(v3.read("Sm(value: 23)") == Some((Sm(23), "")))
    assert(v3.read("Nn") == Some((Nn, "")))

    val v4 = Read[CList[Int]]
    assert(v4.read("CCons(hd: 1, tl: CCons(hd: 2, tl: CCons(hd: 3, tl: CNil)))") == Some((CCons(1, CCons(2, CCons(3, CNil))), "")))

    val v5 = Read[Order[Id]]
    assert(v5.read("""Order(item: "Epoisse", quantity: 10)""") == Some((Order[Id]("Epoisse", 10), "")))
  }

  @Test
  def transform: Unit = {
    val v0 = Transform[BI, ISB]
    assert(v0(BI(true, 23)) == ISB(23, "", true))
  }

  @Test
  def repr: Unit = {
    val v0 = K0.ProductGeneric[Box[Int]]
    val v1 = v0.toRepr(Box(23))
    val v1a: Tuple1[Int] = v1
    assert(v1 == Tuple1(23))

    val v2 = K0.ProductGeneric[Order[Id]]
    val v3 = v2.toRepr(Order[Id]("Epoisse", 10))
    val v3a: (String, Int) = v3
    assert(v3 == ("Epoisse", 10))
  }
}

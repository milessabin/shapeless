/*
 * Copyright (c) 2014 Georgi Krastev (@joroKr21)
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
package shapeless.examples

import scala.language.dynamics

import shapeless._
import shapeless.labelled._
import shapeless.ops.hlist.Tupler
import shapeless.ops.union.Selector
import shapeless.record._
import shapeless.syntax.DynamicRecordOps
import shapeless.tag.@@

/**
 * Example adapted from the talk "Recursion Schemes by Example" by Tim Williams.
 *
 * The idea is that we can represent recursive Algebraic Data Types (ADTs) as the fixpoint of
 * functors by replacing recursive occurrences with a type parameter. This gives us the power to
 * define morphisms in a generic way that encode specific recursion schemes such as the popular
 * `fold` and `unfold` among others.
 *
 * However it might get tedious to define our data schema twice - once as an ADT and once as a
 * functor in addition to the mapping between them. This example shows how we can utilize shapeless
 * to automatically generate the functor representation and mapping to it for a simple recursive
 * data type.
 */
package recursionschemes {

  /** Fix-point of functors. */
  case class Fix[F[_]](unfix: F[Fix[F]])

  /** The constant functor. */
  case class ConstF[A, B](value: A)

  /*
   * In a full implementation the `Functor`, `Applicative`, `Monoid` and `Traverse`  type classes
   * below would be provided by an FP library such as Cats or Scalaz.
   */

  trait Functor[F[_]] {
    def map[A, B](fa: F[A])(f: A => B): F[B]
  }

  trait Applicative[F[_]] extends Functor[F] {
    def pure[A](a: A): F[A]
    def app[A, B](fa: F[A])(f: F[A => B]): F[B]
    def map[A, B](fa: F[A])(f: A => B): F[B] = app(fa)(pure(f))
    def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
      app(fb)(app(fa)(pure(f.curried)))
  }

  object Applicative {
    implicit val idApp: Applicative[Id] = new Applicative[Id] {
      def pure[A](a: A): A = a
      def app[A, B](a: A)(f: A => B): B = f(a)
    }

    implicit val optApp: Applicative[Option] = new Applicative[Option] {
      def pure[A](a: A): Option[A] = Some(a)
      def app[A, B](oa: Option[A])(of: Option[A => B]): Option[B] =
        for (a <- oa; f <- of) yield f(a)
    }

    implicit def constApp[T](implicit T: Monoid[T]) =
      new Applicative[({ type F[x] = ConstF[T, x] })#F] {
        def pure[A](a: A): ConstF[T, A] = ConstF(T.empty)
        def app[A, B](ca: ConstF[T, A])(cf: ConstF[T, A => B]): ConstF[T, B] =
          ConstF(T.combine(ca.value, cf.value))
      }
  }

  trait Traverse[F[_]] extends Functor[F] {
    def traverse[G[_]: Applicative, A, B](fa: F[A])(f: A => G[B]): G[F[B]]
    def sequence[G[_]: Applicative, A](fg: F[G[A]]): G[F[A]] = traverse(fg)(identity)
    def map[A, B](fa: F[A])(f: A => B): F[B] = traverse[Id, A, B](fa)(f)
    def fold[A: Monoid](fa: F[A]): A =
      traverse[({ type G[x] = ConstF[A, x] })#G, A, A](fa)(ConstF(_)).value
  }

  object Traverse {
    def apply[F[_]](implicit F: Traverse[F]): Traverse[F] = F
    implicit val seqTrav: Traverse[Seq] = new Traverse[Seq] {
      def traverse[G[_], A, B](fa: Seq[A])(f: A => G[B])(
        implicit G: Applicative[G]
      ): G[Seq[B]] = fa.foldRight(G.pure(Seq.empty[B])) {
        (h, gt) => G.map2(f(h), gt)(_ +: _)
      }
    }
  }

  trait Monoid[A] {
    def empty: A
    def combine(x: A, y: A): A
  }

  object Monoid {
    implicit val sumMonoid: Monoid[Int] = new Monoid[Int] {
      def empty = 0
      def combine(x: Int, y: Int) = x + y
    }

    implicit def optMonoid[A](implicit A: Monoid[A]): Monoid[Option[A]] = new Monoid[Option[A]] {
      def empty = Some(A.empty)
      def combine(x: Option[A], y: Option[A]) =
        for (ax <- x; ay <- y) yield A.combine(ax, ay)
    }

    implicit def unionMonoid[A]: Monoid[Set[A]] = new Monoid[Set[A]] {
      def empty = Set.empty
      def combine(x: Set[A], y: Set[A]) = x | y
    }

    implicit def overwriteMonoid[K, V]: Monoid[Map[K, V]] = new Monoid[Map[K, V]] {
      def empty = Map.empty
      def combine(x: Map[K, V], y: Map[K, V]) = x ++ y
    }
  }

  /**
   * Even though `T` is not a type constructor, we can mechanically derive a functor `F[_]`
   * representation of `T` that replaces recursive occurrences of `T` with a type parameter.
   *
   * This will allow us to use morphisms as we know them from recursion schemes on the recursive
   * ADT without having to duplicate the data definition a second time in parametrized form.
   */
  trait Morph[T] {
    type F[_]

    type Alg[A] = F[A] => A
    type Coalg[A] = A => F[A]

    /**
     * Performs one step of the conversion `Fix[F] => T`.
     * "Ties" the recursive knot of `T`.
     */
    def tie(ft: F[T]): T

    /**
     * Performs one step of the conversion `T => Fix[F]`.
     * "Unties" the recursive knot of `T`.
     */
    def untie(t: T): F[T]

    /**
     * In a full implementation `traverse` would be provided by the `Traverse` type class,
     * which can be automatically derived for `F[_]`.
     */
    def traverse[G[_]: Applicative, A, B](fa: F[A])(g: A => G[B]): G[F[B]]

    def map[A, B](fa: F[A])(f: A => B): F[B] =
      traverse[Id, A, B](fa)(f)

    def fold[A: Monoid](fa: F[A]): A =
      traverse[({ type G[x] = ConstF[A, x] })#G, A, A](fa)(ConstF(_)).value

    def fixPoint(t: T): Fix[F] =
      cata(t)(Fix.apply)

    def collapse(fix: Fix[F]): T =
      ana(fix)(_.unfix)

    /*
     * In a full implementation the recursion schemes below would be implemented in a stack-safe
     * manner (e.g. using `cats.Eval` or `scalaz.Trampoline`).
     */

    def cata[A](t: T)(alg: Alg[A]): A =
      alg(map(untie(t))(cata(_)(alg)))

    def ana[A](a: A)(coalg: Coalg[A]): T =
      tie(map(coalg(a))(ana(_)(coalg)))

    def hylo[A, B](a: A)(alg: Alg[B])(coalg: Coalg[A]): B =
      alg(map(coalg(a))(hylo(_)(alg)(coalg)))

    def para[A](t: T)(alg: F[(A, T)] => A): A =
      alg(map(untie(t))(x => para(x)(alg) -> x))

    def apo[A](a: A)(coalg: A => F[Either[A, T]]): T =
      tie(map(coalg(a))(_.fold(apo(_)(coalg), identity)))

    def zygo[A, B](t: T)(f: F[B] => B)(g: F[(A, B)] => A): A =
      cata[(A, B)](t)(fab => g(fab) -> f(map(fab)(_._2)))._1

    def cataTrace[A](t: T)(alg: Alg[A]): Map[T, A] =
      para[Map[T, A]](t) { fmt =>
        val fm = map(fmt)(_._1)
        val ft = map(fmt)(_._2)
        val m = fold(fm)
        m + (tie(ft) -> alg(map(ft)(m)))
      }

    /** Syntax sugar for defining algebras. */
    object F extends Dynamic {
      import shapeless.ops.record.Values

      trait Extractor[K <: Symbol] {
        object as {
          def unapply[C <: Coproduct, R <: HList](c: C)(
            implicit sel: Selector.Aux[C, K, R]
          ): Option[DynamicRecordOps[R]] = sel(c).map(_.record)
        }

        object tupled {
          def unapply[C <: Coproduct, R <: HList, V <: HList, P](c: C)(
            implicit sel: Selector.Aux[C, K, R], vals: Values.Aux[R, V], tuple: Tupler.Aux[V, P]
          ): Option[P] = for (r <- sel(c)) yield tuple(vals(r))
        }

        object seq {
          def unapplySeq[C <: Coproduct, S, A](c: C)(
            implicit sel: Selector.Aux[C, K, S], ev: S <:< (Seq[A] :: HNil)
          ): Option[Seq[A]] = sel(c).map(_.head)
        }
      }

      private val extractor = new Extractor[Symbol] { }
      def selectDynamic(name: String): Extractor[Symbol @@ name.type] =
        extractor.asInstanceOf[Extractor[Symbol @@ name.type]]
    }
  }

  object Morph {
    type Aux[T, G[_]] = Morph[T] { type F[x] = G[x] }
    def apply[T](implicit T: Morph[T]): Aux[T, T.F] = T

    /** Helper for recursively deriving `Morph[T]`. */
    trait Rec[T, U] {
      type F[_]
      def tie(ft: F[T]): U
      def untie(u: U): F[T]
      def traverse[G[_]: Applicative, A, B](fa: F[A])(f: A => G[B]): G[F[B]]
    }

    trait RecHList[T, U] extends Rec[T, U] { type F[_] <: HList }
    trait RecCoproduct[T, U] extends Rec[T, U] { type F[_] <: Coproduct }

    object Rec extends Rec1 {
      implicit def reflexive[T] = new Rec[T, T] {
        type F[x] = x
        def tie(t: T): T = t
        def untie(t: T): T = t
        def traverse[G[_]: Applicative, A, B](a: A)(f: A => G[B]): G[B] = f(a)
      }

      implicit def traverse[H[_], T, U](
        implicit H: Traverse[H], U: Rec[T, U]
      ) = new Rec[T, H[U]] {
        type F[x] = H[U.F[x]]
        def tie(ft: F[T]): H[U] = H.map(ft)(U.tie)
        def untie(hu: H[U]): F[T] = H.map(hu)(U.untie(_))
        def traverse[G[_], A, B](fa: F[A])(f: A => G[B])(
          implicit G: Applicative[G]
        ): G[F[B]] = H.traverse(fa)(U.traverse(_)(f))
      }
    }

    trait Rec1 extends Rec2 {
      implicit def fieldType[T, K, V](
        implicit V: Rec[T, V]
      ) = new Rec[T, FieldType[K, V]] {
        type F[x] = FieldType[K, V.F[x]]
        val key = field[K]
        def tie(ft: F[T]): FieldType[K, V] = key[V](V.tie(ft: V.F[T]))
        def untie(kv: FieldType[K, V]): F[T] = key[V.F[T]](V.untie(kv))
        def traverse[G[_], A, B](fa: F[A])(f: A => G[B])(
          implicit G: Applicative[G]
        ): G[F[B]] = G.map(V.traverse(fa: V.F[A])(f))(key[V.F[B]](_))
      }

      implicit def hnil[T] = new RecHList[T, HNil] {
        type F[x] = HNil
        def tie(nil: HNil): HNil = nil
        def untie(nil: HNil): HNil = nil
        def traverse[G[_], A, B](nil: HNil)(f: A => G[B])(
          implicit G: Applicative[G]
        ): G[HNil] = G.pure(nil)
      }

      implicit def cnil[T] = new RecCoproduct[T, CNil] {
        type F[x] = CNil
        def tie(nil: CNil): CNil = nil
        def untie(nil: CNil): CNil = nil
        def traverse[G[_], A, B](nil: CNil)(f: A => G[B])(
          implicit G: Applicative[G]
        ): G[CNil] = G.pure(nil)
      }

      implicit def hcons[U, H, T <: HList](
        implicit H: Strict[Rec[U, H]], T: RecHList[U, T]
      ) = new RecHList[U, H :: T] {
        type F[x] = H.value.F[x] :: T.F[x]
        def tie(fu: F[U]): H :: T = H.value.tie(fu.head) :: T.tie(fu.tail)
        def untie(l: H :: T): F[U] = H.value.untie(l.head) :: T.untie(l.tail)
        def traverse[G[_], A, B](fa: F[A])(f: A => G[B])(implicit G: Applicative[G]): G[F[B]] =
          G.map2(H.value.traverse(fa.head)(f), T.traverse(fa.tail)(f))(_ :: _)
      }

      implicit def ccons[T, L, R <: Coproduct](
        implicit L: Strict[Rec[T, L]], R: RecCoproduct[T, R]
      ) = new RecCoproduct[T, L :+: R] {
        type F[t] = L.value.F[t] :+: R.F[t]

        def tie(ft: F[T]): L :+: R = ft match {
          case Inl(fl) => Inl(L.value.tie(fl))
          case Inr(fr) => Inr(R.tie(fr))
        }

        def untie(c: L :+: R): F[T] = c match {
          case Inl(l) => Inl(L.value.untie(l))
          case Inr(r) => Inr(R.untie(r))
        }

        def traverse[G[_], A, B](fa: F[A])(f: A => G[B])(
          implicit G: Applicative[G]
        ): G[F[B]] = fa match {
          case Inl(fl) => G.map(L.value.traverse(fl)(f))(Inl.apply)
          case Inr(fr) => G.map(R.traverse(fr)(f))(Inr.apply)
        }
      }

      implicit def generic[T, U, R](
        implicit gen: LabelledGeneric.Aux[U, R], R: Rec[T, R]
      ) = new Rec[T, U] {
        type F[x] = R.F[x]
        def tie(ft: F[T]): U = gen.from(R.tie(ft))
        def untie(u: U): F[T] = R.untie(gen.to(u))
        def traverse[G[_], A, B](fa: F[A])(f: A => G[B])(
          implicit G: Applicative[G]
        ): G[F[B]] = R.traverse(fa)(f)
      }
    }

    trait Rec2 {
      implicit def const[T, U] = new Rec[T, U] {
        type F[x] = U
        def tie(u: U): U = u
        def untie(u: U): U = u
        def traverse[G[_], A, B](u: U)(f: A => G[B])(
          implicit G: Applicative[G]
        ): G[U] = G.pure(u)
      }
    }

    implicit def morph[T, R](
      implicit gen: LabelledGeneric.Aux[T, R], R: Rec[T, R]
    ) = new Morph[T] {
      type F[x] = R.F[x]
      def tie(ft: F[T]): T = gen.from(R.tie(ft))
      def untie(t: T): F[T] = R.untie(gen.to(t))
      def traverse[G[_], A, B](fa: F[A])(f: A => G[B])(
        implicit G: Applicative[G]
      ): G[F[B]] = R.traverse(fa)(f)
    }
  }
}

package recursionschemes.examples {
  sealed trait Expr
  case class Num(const: Int) extends Expr
  case class Var(name: String) extends Expr
  case class Add(a: Expr, b: Expr) extends Expr
  case class Mul(a: Expr, b: Expr) extends Expr
  case class Ifn(cond: Expr, a: Expr, b: Expr) extends Expr
  case class Sum(xs: Expr*) extends Expr

  object Expr { /*_*/
    import recursionschemes._
    val morph = Morph[Expr]
    import morph._

    def evalAlg(env: Map[String, Int]): Alg[Option[Int]] = {
      case F.Num.as(num) => Some(num.const)
      case F.Var.as(x) => env.get(x.name)
      case F.Add.as(add) =>
        for (a <- add.a; b <- add.b) yield a + b
      case F.Mul.as(mul) =>
        for (a <- mul.a; b <- mul.b) yield a * b
      case F.Ifn.as(ifn) =>
        for (c <- ifn.cond; x <- if (c < 0) ifn.a else ifn.b) yield x
      case F.Sum.as(sum) =>
        Traverse[Seq].fold(sum.xs)
    }

    def eval(env: Map[String, Int], expr: Expr) =
      cata(expr)(evalAlg(env))

    def pprint(expr: Expr) = cata[String](expr) {
      case F.Num.as(num) => num.const.toString
      case F.Var.as(x) => x.name
      case F.Add.as(add) =>
        s"(${add.a} + ${add.b})"
      case F.Mul.as(mul) =>
        s"(${mul.a} * ${mul.b})"
      case F.Ifn.as(ifn) =>
        s"(${ifn.cond} < 0 ? ${ifn.a} : ${ifn.b})"
      case F.Sum.as(sum) =>
        s"Σ{${sum.xs.mkString(", ")}}"
    }

    def freeVars(expr: Expr) = cata[Set[String]](expr) {
      case F.Var.as(x) => Set(x.name)
      case e => fold(e)
    }

    def subst(map: Map[String, Expr], expr: Expr) = cata[Expr](expr) {
      case v@F.Var.as(x) => map.getOrElse(x.name, tie(v))
      case e => tie(e)
    }

    val optAdd: Alg[Expr] = {
      case F.Add.tupled(Num(0), e) => e
      case F.Add.tupled(e, Num(0)) => e
      case F.Sum.seq() => Num(0)
      case F.Sum.seq(e) => e
      case e => tie(e)
    }

    val optMul: Alg[Expr] = {
      case F.Mul.tupled(Num(1), e) => e
      case F.Mul.tupled(e, Num(1)) => e
      case e => tie(e)
    }

    def optFast(expr: Expr) = cata[Expr](expr) {
      optAdd andThen untie andThen optMul
    }
  }
}

/** examplesJVM/runMain shapeless.examples.RecursionSchemesExamples */
object RecursionSchemesExamples extends App {
  import recursionschemes.examples._
  import Expr._

  def show(v: Option[Int]): String = v.fold("∅")(_.toString)
  val expr = Mul(Ifn(Mul(Num(1), Var("a")), Add(Var("b"), Num(0)), Add(Var("b"), Num(2))), Num(3))
  val sum = Sum(Var("a"), Var("b"), Var("c"))
  val env = Map("a" -> 1, "b" -> 3)
  val opt = optFast(expr)
  val trace = morph.cataTrace(opt)(evalAlg(env)).map {
    case (e, v) => s"${pprint(e)} = ${show(v)}"
  }.mkString("; ")

  println(s"env: $env")
  println(s"expr: ${pprint(expr)} = ${show(eval(env, expr))}")
  println(s"free: ${freeVars(expr)}")
  println(s"free[b -> a]: ${freeVars(subst(Map("b" -> Var("a")), expr))}")
  println(s"opt: ${pprint(opt)}")
  println(s"trace: $trace")
  println(s"sum: ${pprint(sum)} = ${show(eval(env, sum))}")
}

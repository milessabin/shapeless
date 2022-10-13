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

import scala.language.dynamics

import labelled.{ FieldType, field }
import ops.coproduct.{ Inject, Selector => CSelector }
import ops.hlist.{ At, Init, Last, Prepend, Selector, ReplaceAt, Replacer, Tupler }
import ops.record.{ Selector => RSelector, Updater }
import tag.@@

trait Lens[S, A] extends LPLens[S, A] { outer =>
  def get(s: S): A
  def set(s: S)(a: A): S
  def modify(s: S)(f: A => A): S = set(s)(f(get(s)))

  def compose[T](g: Lens[T, S]) = new Lens[T, A] {
    def get(t: T): A = outer.get(g.get(t))
    def set(t: T)(a: A): T = g.modify(t)(outer.set(_)(a))
  }

  def compose[T](g: Prism[T, S]) = new Prism[T, A] {
    def get(t: T): Option[A] = g.get(t).map(outer.get)
    def set(t: T)(a: A): T = g.modify(t)(outer.set(_)(a))
  }

  def >>(n: Nat)(implicit mkLens: MkNthFieldLens[A, n.N]): Lens[S, mkLens.Elem] = mkLens() compose this

  def >>(k: Witness)(implicit mkLens: MkFieldLens[A, k.T]): Lens[S, mkLens.Elem] = mkLens() compose this

  def selectDynamic(k: String)
    (implicit mkLens: MkSelectDynamicOptic[Lens[S, A], A, Symbol @@ k.type, Nothing]): mkLens.Out = mkLens(this)

  def apply[B](implicit mkPrism: MkCtorPrism[A, B]): Prism[S, B] = mkPrism() compose this

  def unapply(s: S): Option[A] = Some(get(s))

  def ~[B](other: Lens[S, B]) = new ProductLensBuilder[S, (A, B)] {
    def get(s: S): (A, B) = (outer.get(s), other.get(s))
    def set(s: S)(ab: (A, B)) = other.set(outer.set(s)(ab._1))(ab._2)
  }

  def ~[B](other: Prism[S, B]) = new ProductPrismBuilder[S, (A, B)] {
    def get(s: S): Option[(A, B)] = other.get(s).map((outer.get(s), _))
    def set(s: S)(ab: (A, B)) = other.set(outer.set(s)(ab._1))(ab._2)
  }
}

trait LPLens[S, A] extends Dynamic with Serializable { self: Lens[S, A] =>
  def selectDynamic[B](k: String)
    (implicit mkLens: MkSelectDynamicOptic[Lens[S, A], A, Symbol @@ k.type, B], dummy: DummyImplicit): mkLens.Out = mkLens(this)
}

trait Prism[S, A] extends LPPrism[S, A] { outer =>
  def get(s: S): Option[A]
  def set(s: S)(a: A): S
  def modify(s: S)(f: A => A): S = get(s).map(f).map(a => set(s)(a)).getOrElse(s)

  def compose[T](g: Lens[T, S]) = new Prism[T, A] {
    def get(t: T): Option[A] = outer.get(g.get(t))
    def set(t: T)(a: A): T = g.modify(t)(outer.set(_)(a))
  }

  def compose[T](g: Prism[T, S]) = new Prism[T, A] {
    def get(t: T): Option[A] = g.get(t).flatMap(outer.get)
    def set(t: T)(a: A): T = g.modify(t)(outer.set(_)(a))
  }

  def selectDynamic(k: String)
    (implicit mkPrism: MkSelectDynamicOptic[Prism[S, A], A, Symbol @@ k.type, Nothing]): mkPrism.Out = mkPrism(this)

  def apply[B](implicit mkPrism: MkCtorPrism[A, B]): Prism[S, B] = mkPrism() compose this

  def unapply(s: S): Option[A] = get(s)

  def ~[B](other: Lens[S, B]) = new ProductPrismBuilder[S, (A, B)] {
    def get(s: S): Option[(A, B)] = outer.get(s).map((_, other.get(s)))

    def set(s: S)(ab: (A, B)) = other.set(outer.set(s)(ab._1))(ab._2)
  }

  def ~[B](other: Prism[S, B]) = new ProductPrismBuilder[S, (A, B)] {
    def get(s: S): Option[(A, B)] =
      for {
        fst <- outer.get(s)
        snd <- other.get(s)
      } yield (fst, snd)

    def set(s: S)(ab: (A, B)) = other.set(outer.set(s)(ab._1))(ab._2)
  }
}

trait LPPrism[S, A] extends Dynamic with Serializable { self: Prism[S, A] =>
  def selectDynamic[B](k: String)
    (implicit mkPrism: MkSelectDynamicOptic[Prism[S, A], A, Symbol @@ k.type, B], dummy: DummyImplicit): mkPrism.Out = mkPrism(this)
}

trait ProductLensBuilder[C, P <: Product] extends Lens[C, P] with Serializable {
  outer =>
  def ~[T, L <: HList, LT <: HList, Q <: Product, QL <: HList](other: Lens[C, T])
    (implicit
      genp: Generic.Aux[P, L],
      tpp: Tupler.Aux[L, P],
      pre: Prepend.Aux[L, T :: HNil, LT],
      tpq: Tupler.Aux[LT, Q],
      genq: Generic.Aux[Q, QL],
      init: Init.Aux[QL, L],
      last: Last.Aux[QL, T]) =
      new ProductLensBuilder[C, Q] {
        def get(c: C): Q = (genp.to(outer.get(c)) :+ other.get(c)).tupled
        def set(c: C)(q: Q) = {
          val l = genq.to(q)
          other.set(outer.set(c)(l.init.tupled))(l.last)
        }
      }
}

trait ProductPrismBuilder[C, P <: Product] extends Prism[C, P] with Serializable {
  outer =>
  def ~[T, L <: HList, LT <: HList, Q <: Product, QL <: HList](other: Prism[C, T])
    (implicit
      genp: Generic.Aux[P, L],
      tpp: Tupler.Aux[L, P],
      pre: Prepend.Aux[L, T :: HNil, LT],
      tpq: Tupler.Aux[LT, Q],
      genq: Generic.Aux[Q, QL],
      init: Init.Aux[QL, L],
      last: Last.Aux[QL, T]) =
      new ProductPrismBuilder[C, Q] {
        def get(c: C): Option[Q] = 
          for {
            init <- outer.get(c)
            last <- other.get(c)
          } yield (genp.to(init) :+ last).tupled

        def set(c: C)(q: Q) = {
          val l = genq.to(q)
          other.set(outer.set(c)(l.init.tupled))(l.last)
        }
      }
}

object OpticDefns {
  def apply[C] = id[C]

  object compose extends Poly2 {
    implicit def default[A, B, C] = at[Lens[B, C], Lens[A, B]](_ compose _)
  }

  class RootLens[C] extends Lens[C, C] {
    def apply[P <: HList](path: Path[P])(implicit mkPath: MkPathOptic[C, P]): mkPath.Out = mkPath()

    def get(c: C): C = c
    def set(c: C)(f: C): C = f
  }

  def id[C] = new RootLens[C]

  def setLens[E](e: E) =
    new Lens[Set[E], Boolean] {
      def get(s: Set[E]): Boolean = s contains e
      def set(s: Set[E])(b: Boolean): Set[E] = if(b) s+e else s-e
    }

  def mapLens[K, V](k: K) =
    new Lens[Map[K, V], Option[V]] {
      def get(m: Map[K, V]): Option[V] = m get k
      def set(m: Map[K, V])(ov: Option[V]): Map[K, V] = ov match {
        case Some(v) => m+(k -> v)
        case None => m-k
      }
    }

  def mapPrism[K, V](k: K) =
    new Prism[Map[K, V], V] {
      def get(m: Map[K, V]): Option[V] = m get k
      def set(m: Map[K, V])(v: V): Map[K, V] = m+(k -> v)
    }

  def hlistSelectLens[L <: HList, U](implicit mkLens: MkHListSelectLens[L, U]) = mkLens()

  def coproductSelectPrism[C <: Coproduct, T](implicit mkPrism: MkCoproductSelectPrism[C, T]) = mkPrism()

  def hlistNthLens[L <: HList, N <: Nat](implicit mkLens: MkHListNthLens[L, N]) = mkLens()

  def recordLens[R <: HList](k: Witness)(implicit mkLens: MkRecordSelectLens[R, k.T]) = mkLens()
}

trait OpticComposer[L, R] extends Serializable {
  type Out
  def apply(l: L, r: R): Out
}

object OpticComposer {
  type Aux[L, R, Out0] = OpticComposer[L, R] { type Out = Out0 }

  implicit def composeLL[S, A, T]: Aux[Lens[S, A], Lens[T, S], Lens[T, A]] =
    new OpticComposer[Lens[S, A], Lens[T, S]] {
      type Out = Lens[T, A]
      def apply(l: Lens[S, A], r: Lens[T, S]): Lens[T, A] = l compose r
    }

  implicit def composeLP[S, A, T]: Aux[Lens[S, A], Prism[T, S], Prism[T, A]] =
    new OpticComposer[Lens[S, A], Prism[T, S]] {
      type Out = Prism[T, A]
      def apply(l: Lens[S, A], r: Prism[T, S]): Prism[T, A] = l compose r
    }

  implicit def composePL[S, A, T]: Aux[Prism[S, A], Lens[T, S], Prism[T, A]] =
    new OpticComposer[Prism[S, A], Lens[T, S]] {
      type Out = Prism[T, A]
      def apply(l: Prism[S, A], r: Lens[T, S]): Prism[T, A] = l compose r
    }

  implicit def composePP[S, A, T]: Aux[Prism[S, A], Prism[T, S], Prism[T, A]] =
    new OpticComposer[Prism[S, A], Prism[T, S]] {
      type Out = Prism[T, A]
      def apply(l: Prism[S, A], r: Prism[T, S]): Prism[T, A] = l compose r
    }
}

trait MkFieldLens[A, K] extends Serializable {
  type Elem
  def apply(): Lens[A, Elem]
}

object MkFieldLens {
  type Aux[A, K, Elem0] = MkFieldLens[A, K] { type Elem = Elem0 }

  implicit def mkFieldLens[A, K, R <: HList, B]
    (implicit
      mkGen: MkLabelledGenericLens.Aux[A, R],
      mkLens: MkRecordSelectLens[R, K]): Aux[A, K, mkLens.Elem] =
        new MkFieldLens[A, K] {
          type Elem = mkLens.Elem
          def apply(): Lens[A, mkLens.Elem] = mkLens() compose mkGen()
        }
}

trait MkNthFieldLens[A, N <: Nat] extends Serializable {
  type Elem
  def apply(): Lens[A, Elem]
}

object MkNthFieldLens {
  type Aux[A, N <: Nat, Elem0] = MkNthFieldLens[A, N] { type Elem = Elem0 }

  implicit def mkGenPNth[A, N <: Nat, R <: HList, B]
    (implicit
      mkGen: MkGenericLens.Aux[A, R],
      mkLens: MkHListNthLens[R, N]): Aux[A, N, mkLens.Elem] =
        new MkNthFieldLens[A, N] {
          type Elem = mkLens.Elem
          def apply(): Lens[A, mkLens.Elem] = mkLens() compose mkGen()
        }
}

trait MkCtorPrism[A, B] extends Serializable {
  def apply(): Prism[A, B]
}

object MkCtorPrism {
  implicit def mkCtorPrism[A, R <: Coproduct, B]
    (implicit
      mkGen: MkGenericLens.Aux[A, R],
      mkPrism: MkCoproductSelectPrism[R, B]): MkCtorPrism[A, B] =
      new MkCtorPrism[A, B] {
        def apply(): Prism[A, B] = mkPrism() compose mkGen()
      }
}

trait InferProduct[C <: Coproduct, K] extends Serializable {
  type Prod
}

object InferProduct {
  type Aux[C <: Coproduct, K, P] = InferProduct[C, K] { type Prod = P }

  implicit def inferProduct1[P, R <: HList, T <: Coproduct, K]
    (implicit gen: LabelledGeneric.Aux[P, R], sel: RSelector[R, K]): Aux[P :+: T, K, P] =
      new InferProduct[P :+: T, K] {
        type Prod = P
      }

  implicit def inferProduct2[H, T <: Coproduct, K, P](implicit it: Aux[T, K, P]): Aux[H :+: T, K, P] =
    new InferProduct[H :+: T, K] {
      type Prod = P
    }
}

trait MkSelectDynamicOptic[R, A, K, B] extends Serializable {
  type Out
  def apply(r: R): Out
}

trait LowPriorityMkSelectDynamicOptic {
  type Aux[R, A, K, B, Out0] = MkSelectDynamicOptic[R, A, K, B] { type Out = Out0 }

  implicit def mkInferCtorSelField[R, A, C <: Coproduct, I, K, E]
    (implicit
      gen: Generic.Aux[A, C],
      infer: InferProduct.Aux[C, K, I],
      mkCSel: MkCtorPrism[A, I],
      mkPSel: MkFieldLens.Aux[I, K, E],
      compose: OpticComposer[Prism[A, E], R]
    ): Aux[R, A, K, Nothing, compose.Out] =
      new MkSelectDynamicOptic[R, A, K, Nothing] {
        type Out = compose.Out
        def apply(r: R): Out = compose(mkPSel() compose mkCSel(), r)
      }

  implicit def mkSelFieldCtor[R, A, K, B, C]
    (implicit
      mkPSel: MkFieldLens.Aux[A, K, C],
      mkCSel: MkCtorPrism[C, B],
      compose: OpticComposer[Prism[A, B], R]
    ): Aux[R, A, K, B, compose.Out] =
    new MkSelectDynamicOptic[R, A, K, B] {
      type Out = compose.Out
      def apply(r: R): Out = compose(mkCSel() compose mkPSel(), r)
    }
}

object MkSelectDynamicOptic extends LowPriorityMkSelectDynamicOptic {
  implicit def mkSelField[R, A, K, E]
    (implicit
      mkLens: MkFieldLens.Aux[A, K, E],
      compose: OpticComposer[Lens[A, E], R]
    ): Aux[R, A, K, Nothing, compose.Out] =
    new MkSelectDynamicOptic[R, A, K, Nothing] {
      type Out = compose.Out
      def apply(r: R): Out = compose(mkLens(), r)
    }

  implicit def mkSelCtor[R, A, B]
    (implicit
      mkPrism: MkCtorPrism[A, B],
      compose: OpticComposer[Prism[A, B], R]
    ): Aux[R, A, Nothing, B, compose.Out] =
    new MkSelectDynamicOptic[R, A, Nothing, B] {
      type Out = compose.Out
      def apply(r: R): Out = compose(mkPrism(), r)
    }
}

trait MkGenericLens[T] extends Serializable {
  type Repr
  def apply(): Lens[T, Repr]
}

object MkGenericLens {
  type Aux[T, Repr0] = MkGenericLens[T] { type Repr = Repr0 }

  implicit def mkGenericLens[T](implicit gen: Generic[T]): Aux[T, gen.Repr] =
    new MkGenericLens[T] {
      type Repr = gen.Repr
      def apply(): Lens[T, Repr] =
        new Lens[T, Repr] {
          def get(t: T): Repr = gen.to(t)
          def set(t: T)(r: Repr): T = gen.from(r)
        }
    }
}

trait MkLabelledGenericLens[T] extends Serializable {
  type Repr
  def apply(): Lens[T, Repr]
}

object MkLabelledGenericLens {
  type Aux[T, Repr0] = MkLabelledGenericLens[T] { type Repr = Repr0 }

  implicit def mkLabelledGenericLens[T](implicit gen: LabelledGeneric[T]): Aux[T, gen.Repr] =
    new MkLabelledGenericLens[T] {
      type Repr = gen.Repr
      def apply(): Lens[T, Repr] =
        new Lens[T, Repr] {
          def get(t: T): Repr = gen.to(t)
          def set(t: T)(r: Repr): T = gen.from(r)
        }
    }
}

trait MkHListNthLens[L <: HList, N <: Nat] extends Serializable {
  type Elem
  def apply(): Lens[L, Elem]
}

object MkHListNthLens {
  type Aux[L <: HList, N <: Nat, Elem0] = MkHListNthLens[L, N] { type Elem = Elem0 }

  implicit def mkHListNthLens[L <: HList, N <: Nat, E]
    (implicit atx: At.Aux[L, N, E], replace: ReplaceAt.Aux[L, N, E, (E, L)]): Aux[L, N, E] =
    new MkHListNthLens[L, N] {
      type Elem = E
      def apply(): Lens[L, E] =
        new Lens[L, E] {
          def get(l: L): E = l[N]
          def set(l: L)(e: E): L = l.updatedAt[N](e)
        }
    }
}

trait MkHListSelectLens[L <: HList, U] extends Serializable {
  def apply(): Lens[L, U]
}

object MkHListSelectLens {
  implicit def mKHlistSelectLens[L <: HList, U]
    (implicit selector: Selector[L, U], replacer: Replacer.Aux[L, U, U, (U, L)]): MkHListSelectLens[L, U] =
      new MkHListSelectLens[L, U] {
        def apply(): Lens[L, U] =
          new Lens[L, U] {
            def get(l: L) = selector(l)
            def set(l: L)(u: U) = replacer(l, u)._2
          }
      }
}

trait MkCoproductSelectPrism[C <: Coproduct, T] extends Serializable {
  def apply(): Prism[C, T]
}

object MkCoproductSelectPrism {
  implicit def mKCoproductSelectPrism[C <: Coproduct, T]
    (implicit selector: CSelector[C, T], injector: Inject[C, T]): MkCoproductSelectPrism[C, T] =
      new MkCoproductSelectPrism[C, T] {
        def apply(): Prism[C, T] =
          new Prism[C, T] {
            def get(c: C): Option[T] = selector(c)
            def set(c: C)(t: T): C = injector(t)
          }
      }
}

trait MkRecordSelectLens[R <: HList, K] extends Serializable {
  type Elem
  def apply(): Lens[R, Elem]
}

object MkRecordSelectLens {
  type Aux[R <: HList, K, Elem0] = MkRecordSelectLens[R, K] { type Elem = Elem0 }

  implicit def mkRecordSelectLens[R <: HList, K, E]
    (implicit selector: RSelector.Aux[R, K, E], updater: Updater.Aux[R, FieldType[K, E], R]): Aux[R, K, E] =
      new MkRecordSelectLens[R, K] {
        type Elem = E
        def apply(): Lens[R, E] =
          new Lens[R, E] {
            def get(r: R) = selector(r)
            def set(r: R)(e: E) = updater(r, field[K](e))
          }
      }
}

trait MkPathOptic[S, P <: HList] extends Serializable {
  type Out
  type Elem
  def apply(): Out
}

trait LowPriorityMkPathOptic {
  type Aux[S, P <: HList, Out0, E0] = MkPathOptic[S, P] { type Out = Out0 ; type Elem = E0 }

  type Aux1[S, P <: HList, Out0] = MkPathOptic[S, P] { type Out = Out0 }

  implicit def mkCoselSelPathOptic[S, P <: HList, K, A, C <: Coproduct, I, E, R]
    (implicit
      mkPrefix: Aux[S, P, R, A],
      gen: Generic.Aux[A, C],
      infer: InferProduct.Aux[C, K, I],
      mkPrism: MkCtorPrism[A, I],
      mkLens: MkFieldLens.Aux[I, K, E],
      compose: OpticComposer[Prism[A, E], R]
    ): Aux[S, Select[K] :: P, compose.Out, E] =
      new MkPathOptic[S, Select[K] :: P] {
        type Out = compose.Out
        type Elem = E
        def apply(): compose.Out = compose(mkLens() compose mkPrism(), mkPrefix())
      }
}

object MkPathOptic extends LowPriorityMkPathOptic {
  implicit def mkHNilPathLens[S]: Aux[S, HNil, Lens[S, S], S] =
    new MkPathOptic[S, HNil] {
      type Out = Lens[S, S]
      type Elem = S
      def apply(): Lens[S, S] = lens[S]
    }

  implicit def mkSelPathOptic[S, P <: HList, K, A, E, R]
    (implicit
      mkPrefix: Aux[S, P, R, A],
      mkLens: MkFieldLens.Aux[A, K, E],
      compose: OpticComposer[Lens[A, E], R]
    ): Aux[S, Select[K] :: P, compose.Out, E] =
      new MkPathOptic[S, Select[K] :: P] {
        type Out = compose.Out
        type Elem = E
        def apply(): compose.Out = compose(mkLens(), mkPrefix())
      }

  implicit def mkCoselPathOptic[S, P <: HList, B, A, R]
    (implicit
      mkPrefix: Aux[S, P, R, A],
      mkPrism: MkCtorPrism[A, B],
      compose: OpticComposer[Prism[A, B], R]
    ): Aux[S, Coselect[B] :: P, compose.Out, B] =
      new MkPathOptic[S, Coselect[B] :: P] {
        type Out = compose.Out
        type Elem = B
        def apply(): compose.Out = compose(mkPrism(), mkPrefix())
      }
}

trait Select[T]
trait Coselect[T]

trait Segment[P, S, T <: HList] {
  type Out <: HList
}

trait LowPrioritySegment {
  type Aux[P, S, T <: HList, Out0 <: HList] = Segment[P, S, T] { type Out = Out0 }

  implicit def two[P, S, T <: HList]: Aux[P, S, T, Coselect[S] :: Select[Symbol @@ P] :: T] = new Segment[P, S, T] {
    type Out = Coselect[S] :: Select[Symbol @@ P] :: T
  }
}

object Segment extends LowPrioritySegment {
  implicit def one[P, T <: HList]: Aux[P, Nothing, T, Select[Symbol @@ P] :: T] = new Segment[P, Nothing, T] {
    type Out = Select[Symbol @@ P] :: T
  }
}

trait Path[T <: HList] extends LPPath[T] {
  type P = Path[T]
  type L = T

  type Lens[T, E] = MkPathOptic.Aux1[T, L, shapeless.Lens[T, E]]
  type Prism[T, E] = MkPathOptic.Aux1[T, L, shapeless.Prism[T, E]]

  def apply[H]: Path[Coselect[H] :: T] = new Path[Coselect[H] :: T] {}

  def selectDynamic(h: String)(implicit segment: Segment[h.type, Nothing, T]): Path[segment.Out] =
    new Path[segment.Out] {}
}

trait LPPath[T <: HList] extends Dynamic { self: Path[T] =>
  def selectDynamic[H](h: String)(implicit segment: Segment[h.type, H, T], dummy: DummyImplicit): Path[segment.Out] =
    new Path[segment.Out] {}
}

object Path extends Path[HNil]

/*
 * Copyright (c) 2011 Miles Sabin 
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

/**
 * Conversions between `Tuples` and `HLists`.
 * 
 * The implicit defined by this object enhances `Tuples` (currently up to arity 4) with an `hlisted` method which
 * constructs an equivalently typed [[shapeless.HList]]. This object also provides higher ranked functions for
 * conversion between `Tuples` and `HLists`.
 * 
 * @author Miles Sabin
 */
object Tuples {
  trait TupleOps[L <: HList] {
    def hlisted : L
  }
  
  implicit def tupleOps[T <: Product](t : T)(implicit hlister : HLister[T]) = new TupleOps[hlister.Out] {
    def hlisted = hlister(t)
  }
  
  /**
   * Higher ranked function which converts `Tuples` to `HLists`. 
   */
  object hlisted {
    def apply[T <: Product](t : T)(implicit hlister : HLister[T]) : hlister.Out = hlister(t)
  }
  implicit def hlisted1[T <: Product](implicit hlister : HLister[T]) =
    new Case[hlisted.type, T => hlister.Out](hlister.apply(_))

  /**
   * Monomorphic instantiator for [[shapeless.Tuples.hlisted]].
   */
  implicit def univInstHListed[F, G](h : hlisted.type)(implicit c : Case[hlisted.type, F => G]) : F => G = c.value
  
  /**
   * Higher ranked function which converts `HLists` to `Tuples`. 
   */
  object tupled {
    def apply[L <: HList](l : L)(implicit tupler : Tupler[L]) : tupler.Out = tupler(l)
  }
  implicit def tupled1[L <: HList](implicit tupler : Tupler[L]) =
    new Case[tupled.type, L => tupler.Out](tupler.apply(_))
  
  /**
   * Monomorphic instantiator for [[shapeless.Tuples.tupled]].
   */
  implicit def univInstTupled[F, G](t : tupled.type)(implicit c : Case[tupled.type, F => G]) : F => G = c.value
}

/**
 * Type class supporting conversion of `Tuples` to `HLists`.
 * 
 * @author Miles Sabin
 */
trait HLister[-T <: Product] {
  type Out <: HList
  def apply(t : T) : Out
}
  
/**
 * `HLister` type class instances.
 * 
 * @author Miles Sabin
 */
object HLister {
  implicit def hlister[T <: Product, Out0 <: HList](implicit hlister : HLister0[T, Out0]) = new HLister[T] {
    type Out = Out0
    def apply(t : T) : Out = hlister(t)
  }
  
  type HListerAux[-T <: Product, Out <: HList] = HLister0[T, Out]
  
  trait HLister0[-T <: Product, Out <: HList] {
    def apply(t : T) : Out
  }
  
  implicit def tupleHLister1[A] = new HLister0[Product1[A], A :: HNil] {
    def apply(t : Product1[A]) = t._1 :: HNil
  }
  
  implicit def tupleHLister2[A, B] = new HLister0[Product2[A, B], A :: B :: HNil] {
    def apply(t : Product2[A, B]) = t._1 :: t._2 :: HNil
  }
  
  implicit def tupleHLister3[A, B, C] = new HLister0[Product3[A, B, C], A :: B :: C :: HNil] {
    def apply(t : Product3[A, B, C]) = t._1 :: t._2 :: t._3 :: HNil
  }

  implicit def tupleHLister4[A, B, C, D] = new HLister0[Product4[A, B, C, D], A :: B :: C :: D :: HNil] {
    def apply(t : Product4[A, B, C, D]) = t._1 :: t._2 :: t._3 :: t._4 :: HNil
  }
}

/**
 * Conversions between ordinary functions and `HList` functions.
 * 
 * The implicits defined by this object enhance ordinary functions (resp. HList functions) with an `hlisted` (resp.
 * `unhlisted`) method which creates an equivalently typed `HList` function (resp. ordinary function).
 * 
 * @author Miles Sabin
 */
object Functions {
  trait FnHListOps[HLFn] {
    def hlisted : HLFn
  }
  
  implicit def fnHListOps[F](t : F)(implicit fnHLister : FnHLister[F]) = new FnHListOps[fnHLister.Out] {
    def hlisted = fnHLister(t)
  }

  trait FnUnHListOps[F] {
    def unhlisted : F
  }

  implicit def fnUnHListOps[F](t : F)(implicit fnUnHLister : FnUnHLister[F]) = new FnUnHListOps[fnUnHLister.Out] {
    def unhlisted = fnUnHLister(t)
  }
}

/**
 * Type class supporting conversion of arbitrary functions (currently up to arity 4) to functions of a single `HList`
 * argument. 
 * 
 * @author Miles Sabin
 */
trait FnHLister[F] {
  type Out
  def apply(f : F) : Out
}
  
trait FnHListerAux[F, Out] {
  def apply(f : F) : Out
}
  
/**
 * `FnHLister` type class instances.
 * 
 * @author Miles Sabin
 */
object FnHLister {
  implicit def fnHLister[F, Out0](implicit fnHLister : FnHListerAux[F, Out0]) = new FnHLister[F] {
    type Out = Out0
    def apply(f : F) : Out = fnHLister(f)
  }
}

object FnHListerAux {
  implicit def fnHLister0[R] = new FnHListerAux[() => R, HNil => R] {
    def apply(f : () => R) = (l : HNil) => f()
  }
  
  implicit def fnHLister1[A, R] = new FnHListerAux[A => R, (A :: HNil) => R] {
    def apply(f : A => R) = (l : A :: HNil) => f(l.head)
  }
  
  implicit def fnHLister2[A, B, R] = new FnHListerAux[(A, B) => R, (A :: B :: HNil) => R] {
    def apply(f : (A, B) => R) = (l : A :: B :: HNil) => f(l.head, l.tail.head)
  }
  
  implicit def fnHLister3[A, B, C, R] = new FnHListerAux[(A, B, C) => R, (A :: B :: C :: HNil) => R] {
    def apply(f : (A, B, C) => R) = (l : A :: B :: C :: HNil) => f(l.head, l.tail.head, l.tail.tail.head)
  }
  
  implicit def fnHLister4[A, B, C, D, R] = new FnHListerAux[(A, B, C, D) => R, (A :: B :: C :: D :: HNil) => R] {
    def apply(f : (A, B, C, D) => R) =
      (l : A :: B :: C :: D :: HNil) => f(l.head, l.tail.head, l.tail.tail.head, l.tail.tail.tail.head)
  }
}

/**
 * Type class supporting conversion of functions of a single `HList` argument to ordinary functions (currently up to
 * arity 4). 
 * 
 * @author Miles Sabin
 */
trait FnUnHLister[F] {
  type Out
  def apply(f : F) : Out
}
  
trait FnUnHListerAux[F, Out] {
  def apply(f : F) : Out
}
  
/**
 * `FnUnHLister` type class instances.
 * 
 * @author Miles Sabin
 */
object FnUnHLister {
  implicit def fnUnHLister[F, Out0](implicit fnUnHLister : FnUnHListerAux[F, Out0]) = new FnUnHLister[F] {
    type Out = Out0
    def apply(f : F) : Out = fnUnHLister(f)
  }
}

object FnUnHListerAux {
  implicit def fnUnHLister0[R] = new FnUnHListerAux[HNil => R, () => R] {
    def apply(f : HNil => R) = () => f(HNil)
  }
  
  implicit def fnUnHLister1[A, R] = new FnUnHListerAux[(A :: HNil) => R, A => R] {
    def apply(f : (A :: HNil) => R) = (a : A) => f(a :: HNil)
  }
  
  implicit def fnUnHLister2[A, B, R] = new FnUnHListerAux[(A :: B :: HNil) => R, (A, B) => R] {
    def apply(f : (A :: B :: HNil) => R) = (a : A, b : B) => f(a :: b :: HNil)
  }

  implicit def fnUnHLister3[A, B, C, R] = new FnUnHListerAux[(A :: B :: C :: HNil) => R, (A, B, C) => R] {
    def apply(f : (A :: B :: C :: HNil) => R) = (a : A, b : B, c : C) => f(a :: b :: c :: HNil)
  }

  implicit def fnUnHLister4[A, B, C, D, R] = new FnUnHListerAux[(A :: B :: C :: D :: HNil) => R, (A, B, C, D) => R] {
    def apply(f : (A :: B :: C :: D :: HNil) => R) = (a : A, b : B, c : C, d : D) => f(a :: b :: c :: d :: HNil)
  }
}

/**
 * Conversions between `Traversables` and `HLists`.
 * 
 * The implicit defined by this object enhances `Traversables` with a `toHList` method which constructs an equivalently
 * typed [[shapeless.HList]] if possible. 
 * 
 * @author Miles Sabin
 */
object Traversables {
  trait TraversableOps[T] {
    def toHList[L <: HList](implicit fl : FromTraversable[T, L]) : Option[L]
  }
  
  implicit def traversableOps[T](l : Traversable[T]) = new TraversableOps[T] {
    def toHList[L <: HList](implicit fl : FromTraversable[T, L]) = fl(l) 
  }
}

/**
 * Type class supporting type safe conversion of `Traversables` to `HLists`. 
 * 
 * @author Miles Sabin
 */
trait FromTraversable[T, Out <: HList] {
  def apply(l : Traversable[T]) : Option[Out]
}
  
/**
 * `FromTraversable` type class instances.
 * 
 * @author Miles Sabin
 */
object FromTraversable {
  import Typeable._

  implicit def hnilFromTraversable[T] = new FromTraversable[T, HNil] {
    def apply(l : Traversable[T]) = l match {
      case Nil => Some(HNil)
      case _ => None
    }
  }
  
  implicit def hlistFromTraversable[T, OutH, OutT <: HList]
    (implicit flt : FromTraversable[T, OutT], oc : Typeable[OutH]) = new FromTraversable[T, OutH :: OutT] {
      def apply(l : Traversable[T]) : Option[OutH :: OutT] =
        for(e <- l.headOption; h <- e.cast[OutH]; t <- flt(l.tail)) yield h :: t
  }
}
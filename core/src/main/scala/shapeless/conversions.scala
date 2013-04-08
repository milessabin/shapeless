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
 * The implicit defined by this object enhances `Tuples` with an `hlisted` method which constructs
 * an equivalently typed [[shapeless.HList]]. This object also provides higher ranked functions for
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
  object hlisted extends Poly1 {
    implicit def caseProduct[T <: Product](implicit hlister : HLister[T]) = at[T](hlister(_))
  }

  /**
   * Higher ranked function which converts `HLists` to `Tuples`. 
   */
  object tupled extends Poly1 {
    implicit def caseHList[L <: HList](implicit tupler : Tupler[L]) = at[L](tupler(_))
  }
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
  
trait HListerAux[-T <: Product, Out <: HList] {
  def apply(t : T) : Out
}

/**
 * `HLister` type class instances.
 * 
 * @author Miles Sabin
 */
object HLister {
  implicit def hlister[T <: Product, Out0 <: HList](implicit hlister : HListerAux[T, Out0]) = new HLister[T] {
    type Out = Out0
    def apply(t : T) : Out = hlister(t)
  }
}

object HListerAux extends HListerAuxInstances

/**
 * Type class witnessing the arity of a `Product`
 * 
 * @author Miles Sabin
 */
trait ProductArity[P <: Product] {
  type N <: Nat
}

trait ProductArityAux[P <: Product, N <: Nat]

/**
 * `ProductArity` type class instances.
 * 
 * @author Miles Sabin
 */
object ProductArity {
  implicit def arity[P <: Product, N0 <: Nat](implicit an : ProductArityAux[P, N0]) = new ProductArity[P] {
    type N = N0
  }
}

object ProductArityAux {
  implicit def arityN[P <: Product, L <: HList, N <: Nat]
    (implicit hl : HListerAux[P, L], len : LengthAux[L, N]) = new ProductArityAux[P, N] {} 
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
 * Type class supporting conversion of arbitrary functions to functions of a single `HList`
 * argument. 
 * 
 * @author Miles Sabin
 */
trait FnHLister[F] {
  type Out = Args => Result
  type Args <: HList
  type Result
  
  def apply(f : F) : Out
}
  
trait FnHListerAux[F, Out] {
  type Args <: HList
  type Result
  def apply(f : F) : Out
}

/**
 * `FnHLister` type class instances.
 * 
 * @author Miles Sabin
 */
object FnHLister {
  implicit def fnHLister[F, Args0 <: HList, Result0](implicit fnHLister : FnHListerAux[F, Args0 => Result0]) = new FnHLister[F] {
    type Args = Args0
    type Result = Result0
    def apply(f : F) : Out = fnHLister(f)
  }
}

object FnHListerAux extends FnHListerAuxInstances

/**
 * Type class supporting conversion of functions of a single `HList` argument to ordinary functions. 
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

object FnUnHListerAux extends FnUnHListerAuxInstances

import scala.collection.GenTraversable

/**
 * Conversions between `Traversables` and `HLists`.
 * 
 * The implicit defined by this object enhances `Traversables` with a `toHList` method which constructs an equivalently
 * typed [[shapeless.HList]] if possible. 
 * 
 * @author Miles Sabin
 */
object Traversables {
  
  trait TraversableOps {
    def toHList[L <: HList](implicit fl : FromTraversable[L]) : Option[L]
  }
  
  implicit def traversableOps[T <% GenTraversable[_]](t : T) = new TraversableOps {
    def toHList[L <: HList](implicit fl : FromTraversable[L]) = fl(t) 
  }
}

/**
 * Type class supporting type safe conversion of `Traversables` to `HLists`. 
 * 
 * @author Miles Sabin
 */
trait FromTraversable[Out <: HList] {
  def apply(l : GenTraversable[_]) : Option[Out]
}

/**
 * `FromTraversable` type class instances.
 * 
 * @author Miles Sabin
 */
object FromTraversable {
  import scala.collection.GenTraversableLike
  import Typeable._

  implicit def hnilFromTraversable[T] = new FromTraversable[HNil] {
    def apply(l : GenTraversable[_]) =
      if(l.isEmpty) Some(HNil) else None 
  }
  
  implicit def hlistFromTraversable[OutH, OutT <: HList]
    (implicit flt : FromTraversable[OutT], oc : Typeable[OutH]) = new FromTraversable[OutH :: OutT] {
      def apply(l : GenTraversable[_]) : Option[OutH :: OutT] =
        if(l.isEmpty) None
        else for(h <- l.head.cast[OutH]; t <- flt(l.tail)) yield h :: t
  }
}

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

import scala.collection.{ GenTraversable, GenTraversableLike }
import scala.collection.generic.CanBuildFrom

/**
 * Wrapper for a collection type witnessing that it has the statically specified length. Can be
 * applied to any type which can be viewed as a `GenTraversableLike`, ie. standard collections,
 * `Array`s, `String`s etc.
 * 
 * @author Miles Sabin
 */
abstract class Sized[+Repr, L <: Nat](r : Repr) {
  type A
  
  def unsized = r

  override def toString = r.toString
}

/**
 * Carrier for `Sized` operations.
 * 
 * These methods are implemented here and pimped onto the minimal `Sized` type to avoid issues that would otherwise be
 * caused by its covariance.
 * 
 * @author Miles Sabin
 */
class SizedOps[A, Repr <% GenTraversableLike[A, Repr], L <: Nat](r : Repr) { outer =>
  import Sized._
  import Nat._
  import LT._
  
  /**
   * Returns the head of this collection. Available only if there is evidence that this collection has at least one
   * element.
   */
  def head(implicit ev : _0 < L) : A = r.head
  
  /**
   * Returns the tail of this collection. Available only if there is evidence that this collection has at least one
   * element.
   */
  def tail(implicit pred : Pred[L]) = wrap[A, Repr, pred.Out](r.tail)
  
  /**
   * Returns the first ''m'' elements of this collection. An explicit type argument must be provided. Available only if
   * there is evidence that this collection has at least ''m'' elements. The resulting collection will be statically
   * known to have ''m'' elements.
   */
  def take[M <: Nat](implicit diff : Diff[L, M], ev : ToInt[M]) = wrap[A, Repr, M](r.take(toInt[M]))
  
  /**
   * Returns the first ''m'' elements of this collection. Available only if there is evidence that this collection has
   * at least ''m'' elements. The resulting collection will be statically known to have ''m'' elements.
   */
  def take[M <: Nat](m : M)(implicit diff : Diff[L, M], ev : ToInt[M]) = wrap[A, Repr, M](r.take(toInt[M]))

  /**
   * Returns all but the  first ''m'' elements of this collection. An explicit type argument must be provided. Available
   * only if there is evidence that this collection has at least ''m'' elements. The resulting collection will be 
   * statically known to have ''m'' less elements than this collection.
   */
  def drop[M <: Nat](implicit diff : Diff[L, M], ev : ToInt[M]) = wrap[A, Repr, diff.Out](r.drop(toInt[M]))
  
  /**
   * Returns all but the  first ''m'' elements of this collection. Available only if there is evidence that this
   * collection has at least ''m'' elements. The resulting collection will be statically known to have ''m'' less
   * elements than this collection.
   */
  def drop[M <: Nat](m : M)(implicit diff : Diff[L, M], ev : ToInt[M]) = wrap[A, Repr, diff.Out](r.drop(toInt[M]))
  
  /**
   * Splits this collection at the ''mth'' element, returning the prefix and suffix as a pair. An explicit type argument
   * must be provided. Available only if there is evidence that this collection has at least ''m'' elements. The
   * resulting collections will be statically know to have ''m'' and ''n-m'' elements respectively.
   */
  def splitAt[M <: Nat](implicit diff : Diff[L, M], ev : ToInt[M]) = (take[M], drop[M])
  
  /**
   * Splits this collection at the ''mth'' element, returning the prefix and suffix as a pair. Available only if there
   * is evidence that this collection has at least ''m'' elements. The resulting collections will be statically know to
   * have ''m'' and ''n-m'' elements respectively.
   */
  def splitAt[M <: Nat](m : M)(implicit diff : Diff[L, M], ev : ToInt[M]) = (take[M], drop[M])
  
  /**
   * Prepend the argument element to this collection. The resulting collection will be statically known to have a size
   * one greater than this collection.
   */
  def +:(elem : A)(implicit cbf : CanBuildFrom[Repr, A, Repr]) = {
    val builder = cbf.apply(r)
    builder += elem
    builder ++= r.toIterator
    wrap[A, Repr, Succ[L]](builder.result)
  }
  
  /**
   * Append the argument element to this collection. The resulting collection will be statically known to have a size
   * one greater than this collection.
   */
  def :+(elem : A)(implicit cbf : CanBuildFrom[Repr, A, Repr]) = {
    val builder = cbf.apply(r)
    builder ++= r.toIterator
    builder += elem
    wrap[A, Repr, Succ[L]](builder.result)
  }
  
  /**
   * Append the argument collection to this collection. The resulting collection will be statically known to have
   * ''m+n'' elements.
   */
  def ++[B >: A, That, M <: Nat](that : Sized[That, M] { type A = B })
    (implicit
      sum : Sum[L, M],
      cbf : CanBuildFrom[Repr, B, That],
      convThat : That => GenTraversableLike[B, That]) = wrap[B, That, sum.Out](r ++ that.unsized)
    
  /**
   * Map across this collection. The resulting collection will be statically known to have the same number of elements
   * as this collection.
   */
  def map[B, That](f : A => B)(implicit cbf : CanBuildFrom[Repr, B, That]) = wrap[B, That, L](r map f)
}

trait LowPrioritySized {
  implicit def sizedToRepr[Repr](s : Sized[Repr, _]) : Repr = s.unsized
}

object Sized extends LowPrioritySized {
  import Nat._
  
  implicit def sizedOps[A0, Repr <% GenTraversableLike[A0, Repr], L <: Nat]
    (s : Sized[Repr, L] { type A = A0 }) : SizedOps[A0, Repr, L] = new SizedOps[A0, Repr, L](s.unsized)
  
  def apply[CC[_]] = new SizedBuilder[CC]
  
  def apply[CC[_]]()
    (implicit cbf : CanBuildFrom[Nothing, Nothing, CC[Nothing]]) =
      new Sized[CC[Nothing], _0](cbf().result) { type A = Nothing }
  
  def wrap[A0, Repr, L <: Nat](r : Repr) = new Sized[Repr, L](r) { type A = A0 }

  class SizedConv[A, Repr <% GenTraversableLike[A, Repr]](r : Repr) {
    def sized[L <: Nat](implicit toInt : ToInt[L]) =
      if(r.size == toInt()) Some(wrap[A, Repr, L](r)) else None
      
    def ensureSized[L <: Nat](implicit toInt : ToInt[L]) = {
      assert(r.size == toInt())
      wrap[A, Repr, L](r)
    }
  }
  
  def unapplySeq[A0, Repr <% GenTraversableLike[A0, Repr], L <: Nat]
    (x : Sized[Repr, L] { type A = A0 }) = Some(x.unsized)

  implicit def genTraversableSizedConv[CC[X] <: GenTraversable[X], T](cc : CC[T])
    (implicit conv : CC[T] => GenTraversableLike[T, CC[T]]) = new SizedConv[T, CC[T]](cc)
  
  implicit def stringSizedConv(s : String) = new SizedConv[Char, String](s)
}

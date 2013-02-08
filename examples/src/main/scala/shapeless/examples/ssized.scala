/*
 * Copyright (c) 2013 Miles Sabin 
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

import shapeless._

import scala.collection.{ GenTraversable, GenTraversableLike }
import scala.collection.generic.CanBuildFrom

/**
 * Wrapper for a collection type witnessing that it has the statically specified length. Can be
 * applied to any type which can be viewed as a `GenTraversableLike`, ie. standard collections,
 * `Array`s, `String`s etc.
 * 
 * @author Miles Sabin
 */
abstract class SSized[+Repr, L](r : Repr) {
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
class SSizedOps[A, Repr <% GenTraversableLike[A, Repr], L](r : Repr) { outer =>
  import SSized._
  import SNat._
  import SLt._
  
  /**
   * Returns the head of this collection. Available only if there is evidence that this collection has at least one
   * element.
   */
  def head(implicit ev : SLt[SInt(0), L, SBool(true)]) : A = r.head
  
  /**
   * Returns the tail of this collection. Available only if there is evidence that this collection has at least one
   * element.
   */
  def tail[M](implicit pred : SDiff[L, SInt(1), M]) = wrap[A, Repr, M](r.tail)
  
  /**
   * Returns the first ''m'' elements of this collection. Available only if there is evidence that this collection has
   * at least ''m'' elements. The resulting collection will be statically known to have ''m'' elements.
   */
  def take[M, D](m : SNat[M])(implicit sdiff : SDiff[L, M, D]) = wrap[A, Repr, M](r.take(m.value))

  /**
   * Returns all but the  first ''m'' elements of this collection. Available only if there is evidence that this
   * collection has at least ''m'' elements. The resulting collection will be statically known to have ''m'' less
   * elements than this collection.
   */
  def drop[M, D](m : SNat[M])(implicit sdiff : SDiff[L, M, D]) = wrap[A, Repr, D](r.drop(m.value))
  
  /**
   * Splits this collection at the ''mth'' element, returning the prefix and suffix as a pair. Available only if there
   * is evidence that this collection has at least ''m'' elements. The resulting collections will be statically know to
   * have ''m'' and ''n-m'' elements respectively.
   */
  def splitAt[M, D](m : SNat[M])(implicit sdiff : SDiff[L, M, D]) = (take(m), drop(m))
  
  /**
   * Prepend the argument element to this collection. The resulting collection will be statically known to have a size
   * one greater than this collection.
   */
  def +:[S](elem : A)(implicit succ: SSum[L, SInt(1), S], cbf : CanBuildFrom[Repr, A, Repr]) = {
    val builder = cbf.apply(r)
    builder += elem
    builder ++= r.toIterator
    wrap[A, Repr, S](builder.result)
  }
  
  /**
   * Append the argument element to this collection. The resulting collection will be statically known to have a size
   * one greater than this collection.
   */
  def :+[S](elem : A)(implicit succ: SSum[L, SInt(1), S], cbf : CanBuildFrom[Repr, A, Repr]) = {
    val builder = cbf.apply(r)
    builder ++= r.toIterator
    builder += elem
    wrap[A, Repr, S](builder.result)
  }
  
  /**
   * Append the argument collection to this collection. The resulting collection will be statically known to have
   * ''m+n'' elements.
   */
  def ++[B >: A, That, M, S](that : SSized[That, M] { type A = B })
    (implicit
      sum : SSum[L, M, S],
      cbf : CanBuildFrom[Repr, B, That],
      convThat : That => GenTraversableLike[B, That]) = wrap[B, That, S](r ++ that.unsized)
    
  /**
   * Map across this collection. The resulting collection will be statically known to have the same number of elements
   * as this collection.
   */
  def map[B, That](f : A => B)(implicit cbf : CanBuildFrom[Repr, B, That]) = wrap[B, That, L](r map f)
}

trait LowPrioritySSized {
  implicit def sizedToRepr[Repr](s : Sized[Repr, _]) : Repr = s.unsized
}

object SSized extends LowPrioritySSized {
  import SNat._
  
  implicit def sizedOps[A0, Repr <% GenTraversableLike[A0, Repr], L]
    (s : SSized[Repr, L] { type A = A0 }) : SSizedOps[A0, Repr, L] = new SSizedOps[A0, Repr, L](s.unsized)
  
  def wrap[A0, Repr, L](r : Repr) = new SSized[Repr, L](r) { type A = A0 }

  class SSizedConv[A, Repr <% GenTraversableLike[A, Repr]](r : Repr) {
    def sized[L](l: SNat[L]) =
      if(r.size == l.value) Some(wrap[A, Repr, L](r)) else None
      
    def ensureSized[L](l: SNat[L]) = {
      assert(r.size == l.value)
      wrap[A, Repr, L](r)
    }
  }
  
  def unapplySeq[A0, Repr <% GenTraversableLike[A0, Repr], L]
    (x : SSized[Repr, L] { type A = A0 }) = Some(x.unsized)

  implicit def genTraversableSizedConv[CC[X] <: GenTraversable[X], T](cc : CC[T])
    (implicit conv : CC[T] => GenTraversableLike[T, CC[T]]) = new SSizedConv[T, CC[T]](cc)
  
  implicit def stringSizedConv(s : String) = new SSizedConv[Char, String](s)
}

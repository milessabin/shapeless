/*
 * Copyright (c) 2011-18 Miles Sabin
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

import scala.collection._

/**
 * Wrapper for a collection type witnessing that it has the statically specified length. Can be
 * applied to any type which can be viewed as an `IterableOps`, ie. standard collections,
 * `Array`s, `String`s etc.
 * 
 * @author Miles Sabin
 */
final class Sized[+Repr, L <: Nat] private (val unsized : Repr) {
  // Cannot extend AnyVal in 2.10, see https://issues.scala-lang.org/browse/SI-6260
  override def toString = unsized.toString

  override def equals(other: Any): Boolean =
    other match {
      case o: Sized[_, _] => unsized == o.unsized
      case _ => false
    }

  override def hashCode: Int = unsized.hashCode
}

/**
 * Carrier for `Sized` operations.
 * 
 * These operations are implemented here as extension methods of the minimal `Sized` type to avoid issues that would
 * otherwise be caused by its covariance.
 * 
 * @author Miles Sabin
 */
class SizedOps[A0, Repr : AdditiveCollection, L <: Nat](s : Sized[Repr, L], itl: IsRegularIterable[Repr] { type A = A0 })
  extends SizedOpsVersionSpecific[A0, Repr, L] {

  import nat._
  import ops.nat._
  import LT._
  import Sized.wrap
  import ops.sized._
  import ops.hlist.Tupler

  def underlying: IterableOps[A0, Iterable, Repr] = itl(s.unsized)

  /**
   * Returns the ''nth'' element of this `Sized`. Available only if there is evidence that this `Sized` has at least ''n''
   * elements.
   */
  def apply[N <: Nat](implicit diff: Diff[L, Succ[N]], ev: ToInt[N]): A0 = itl(underlying.drop(toInt[N])).head

  /**
   * Returns the ''nth'' element of this `Sized`. Available only if there is evidence that this `Sized` has at least ''n''
   * elements.
   */
  def apply(n: Nat)(implicit diff: Diff[L, Succ[n.N]], ev: ToInt[n.N]): A0 = apply[n.N]
  
  /**
   * Returns the ''nth'' element of this `Sized`. Available only if there is evidence that this `Sized` has at least ''n''
   * elements.
   */
  def at[N <: Nat](implicit diff: Diff[L, Succ[N]], ev: ToInt[N]): A0 = apply[N]

  /**
   * Returns the ''nth'' element of this `Sized`. Available only if there is evidence that this `Sized` has at least ''n''
   * elements.
   */
  def at(n: Nat)(implicit diff: Diff[L, Succ[n.N]], ev: ToInt[n.N]): A0 = apply[n.N]

  /**
   * Returns the head of this collection. Available only if there is evidence that this collection has at least one
   * element.
   */
  def head(implicit ev : _0 < L) : A0 = underlying.head
  
  /**
   * Returns the tail of this collection. Available only if there is evidence that this collection has at least one
   * element.
   */
  def tail(implicit pred : Pred[L]) = wrap[Repr, pred.Out](underlying.tail)
  
  /**
   * Returns the first ''m'' elements of this collection. An explicit type argument must be provided. Available only if
   * there is evidence that this collection has at least ''m'' elements. The resulting collection will be statically
   * known to have ''m'' elements.
   */
  def take[M <: Nat](implicit diff : Diff[L, M], ev : ToInt[M]) = wrap[Repr, M](underlying.take(toInt[M]))
  
  /**
   * Returns the first ''m'' elements of this collection. Available only if there is evidence that this collection has
   * at least ''m'' elements. The resulting collection will be statically known to have ''m'' elements.
   */
  def take(m : Nat)(implicit diff : Diff[L, m.N], ev : ToInt[m.N]) = wrap[Repr, m.N](underlying.take(toInt[m.N]))

  /**
   * Returns all but the  first ''m'' elements of this collection. An explicit type argument must be provided. Available
   * only if there is evidence that this collection has at least ''m'' elements. The resulting collection will be 
   * statically known to have ''m'' less elements than this collection.
   */
  def drop[M <: Nat](implicit diff : Diff[L, M], ev : ToInt[M]) = wrap[Repr, diff.Out](underlying.drop(toInt[M]))
  
  /**
   * Returns all but the  first ''m'' elements of this collection. Available only if there is evidence that this
   * collection has at least ''m'' elements. The resulting collection will be statically known to have ''m'' less
   * elements than this collection.
   */
  def drop(m : Nat)(implicit diff : Diff[L, m.N], ev : ToInt[m.N]) = wrap[Repr, diff.Out](underlying.drop(toInt[m.N]))
  
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
  def splitAt(m : Nat)(implicit diff : Diff[L, m.N], ev : ToInt[m.N]) = (take[m.N], drop[m.N])
  
  /**
   * Prepend the argument element to this collection. The resulting collection will be statically known to have a size
   * one greater than this collection.
   */
  def +:(elem : A0)(implicit cbf : BuildFrom[Repr, A0, Repr]) = {
    val builder = cbf.newBuilder(s.unsized)
    builder += elem
    builder ++= underlying.iterator
    wrap[Repr, Succ[L]](builder.result())
  }
  
  /**
   * Append the argument element to this collection. The resulting collection will be statically known to have a size
   * one greater than this collection.
   */
   def :+(elem : A0)(implicit cbf : BuildFrom[Repr, A0, Repr]) = {
    val builder = cbf.newBuilder(s.unsized)
    builder ++= underlying.iterator
    builder += elem
    wrap[Repr, Succ[L]](builder.result())
  }

  /**
   * Map across this collection. The resulting collection will be statically known to have the same number of elements
   * as this collection.
   */
  def map[B, That](f : A0 => B)(implicit cbf : BuildFrom[Repr, B, That], ev : AdditiveCollection[That]) = {
    val builder = cbf.newBuilder(s.unsized)
    builder ++= underlying.iterator.map(f)
    wrap[That, L](builder.result())
  }

  /**
   * Converts this `Sized` to an `HList` whose elements have the same type as in `Repr`. 
   */
  def toHList(implicit hl: ToHList[Repr, L]): hl.Out = hl(s)

  /**
   * Converts this `Sized` to a tuple whose elements have the same type as in `Repr`.
   */
  def tupled[L0 <: HList, T](implicit hl: ToHList.Aux[Repr, L, L0], t: Tupler.Aux[L0, T]): T = t(hl(s))
}

trait LowPrioritySized {
  implicit def sizedToRepr[Repr](s : Sized[Repr, _]) : Repr = s.unsized
}

object Sized extends SizedVersionSpecific with LowPrioritySized {

  implicit def sizedOps[Repr, L <: Nat](s: Sized[Repr, L])(
    implicit itl: IsRegularIterable[Repr],
    ev: AdditiveCollection[Repr]
  ): SizedOps[itl.A, Repr, L] =
    new SizedOps[itl.A, Repr, L](s, itl)
  
  def apply[CC[_]] = new SizedBuilder[CC]
  
  def wrap[Repr, L <: Nat](r : Repr)(implicit ev: AdditiveCollection[Repr]): Sized[Repr, L] =
    new Sized[Repr, L](r)

  def unapplySeq[Repr, L <: Nat](x: Sized[Repr, L]): Some[Repr] =
    Some(x.unsized)
}

/**
 * Evidence that `Repr` instances can be nested in a `Sized`.
 *
 * Should assert that a `Builder[_, Repr]` given n elements will result in a Repr of length n.
 *
 * @author Alexandre Archambault
 */
trait AdditiveCollection[Repr] extends Serializable

object AdditiveCollection extends AdditiveCollectionVersionSpecific {
  import scala.collection.immutable.Queue
  import scala.collection.LinearSeq

  private[this] val instance =
    new AdditiveCollection[Any] {}

  implicit def linearSeqAdditiveCollection[T]: AdditiveCollection[LinearSeq[T]] =
    instance.asInstanceOf[AdditiveCollection[LinearSeq[T]]]

  implicit def vectorAdditiveCollection[T]: AdditiveCollection[Vector[T]] =
    instance.asInstanceOf[AdditiveCollection[Vector[T]]]

  implicit def arrayAdditiveCollection[T]: AdditiveCollection[Array[T]] =
    instance.asInstanceOf[AdditiveCollection[Array[T]]]

  implicit def stringAdditiveCollection: AdditiveCollection[String] =
    instance.asInstanceOf[AdditiveCollection[String]]

  implicit def listAdditiveCollection[T]: AdditiveCollection[List[T]] =
    instance.asInstanceOf[AdditiveCollection[List[T]]]

  implicit def streamAdditiveCollection[T]: AdditiveCollection[LazyList[T]] =
    instance.asInstanceOf[AdditiveCollection[LazyList[T]]]

  implicit def queueAdditiveCollection[T]: AdditiveCollection[Queue[T]] =
    instance.asInstanceOf[AdditiveCollection[Queue[T]]]

  implicit def defaultAdditiveCollection[T]: AdditiveCollection[collection.immutable.IndexedSeq[T]] =
    instance.asInstanceOf[AdditiveCollection[collection.immutable.IndexedSeq[T]]]

  implicit def seqAdditiveCollection[T]: AdditiveCollection[Seq[T]] =
    instance.asInstanceOf[AdditiveCollection[Seq[T]]]
}

class DefaultToIndexedSeq[CC[_]]
object DefaultToIndexedSeq {
  implicit def defaultInstance: DefaultToIndexedSeq[collection.immutable.IndexedSeq] = null
  implicit def explicitInstance[CC[_]]: DefaultToIndexedSeq[CC] = null
}

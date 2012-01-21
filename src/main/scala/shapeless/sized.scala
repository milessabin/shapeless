package shapeless

import scala.collection.{ GenTraversable, GenTraversableLike }
import scala.collection.generic.CanBuildFrom

abstract class Sized[+Repr, L <: Nat](r : Repr) {
  type A
  
  def unsized = r

  override def toString = r.toString
}

class SizedOps[A, Repr <% GenTraversableLike[A, Repr], L <: Nat](r : Repr) { outer =>
  import Sized._
  import Nat._
  import LT._
  
  def head(implicit ev : _0 < L) : A = r.head
  
  def tail(implicit pred : Pred[L]) = wrap[A, Repr, pred.Out](r.tail)
  
  def take[M <: Nat](implicit diff : Diff[L, M], ev : ToInt[M]) = wrap[A, Repr, M](r.take(toInt[M]))
  
  def drop[M <: Nat](implicit diff : Diff[L, M], ev : ToInt[M]) = wrap[A, Repr, diff.Out](r.drop(toInt[M]))
  
  def splitAt[M <: Nat](implicit diff : Diff[L, M], ev : ToInt[M]) = (take[M], drop[M])
  
  def +:(elem : A)(implicit cbf : CanBuildFrom[Repr, A, Repr]) = {
    val builder = cbf.apply(r)
    builder += elem
    builder ++= r.toIterator
    wrap[A, Repr, Succ[L]](builder.result)
  }
  
  def :+(elem : A)(implicit cbf : CanBuildFrom[Repr, A, Repr]) = {
    val builder = cbf.apply(r)
    builder ++= r.toIterator
    builder += elem
    wrap[A, Repr, Succ[L]](builder.result)
  }
  
  def ++[B >: A, That, M <: Nat](that : Sized[That, M] { type A = B })
    (implicit
      sum : Sum[L, M],
      cbf : CanBuildFrom[Repr, B, That],
      convThat : That => GenTraversableLike[B, That]) = wrap[B, That, sum.Out](r ++ that.unsized)
    
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

  implicit def genTraversableSizedConv[CC[X] <: GenTraversable[X], T](cc : CC[T])
    (implicit conv : CC[T] => GenTraversableLike[T, CC[T]]) = new SizedConv[T, CC[T]](cc)
  
  implicit def stringSizedConv(s : String) = new SizedConv[Char, String](s)
}

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
  
  type A0 = A
  
  def head(implicit ev : _0 < L) : A = r.head
  
  def tail(implicit pred : Pred[L]) = new Sized[Repr, pred.Out](r.tail) { type A = A0 }
  
  def take[M <: Nat](implicit diff : Diff[L, M], ev : ToInt[M]) = new Sized[Repr, M](r.take(toInt[M])) { type A = A0 }
  
  def drop[M <: Nat](implicit diff : Diff[L, M], ev : ToInt[M]) = new Sized[Repr, diff.Out](r.drop(toInt[M])) { type A = A0 }
  
  def splitAt[M <: Nat](implicit diff : Diff[L, M], ev : ToInt[M]) = (take[M], drop[M])
  
  def +:(elem : A)(implicit cbf : CanBuildFrom[Repr, A, Repr]) = {
    val builder = cbf.apply(r)
    builder += elem
    builder ++= r.toIterator
    new Sized[Repr, Succ[L]](builder.result) { type A = A0 }
  }
  
  def ++[B >: A, That, M <: Nat](that : Sized[That, M] { type A = B })
    (implicit
      sum : Sum[L, M],
      cbf : CanBuildFrom[Repr, B, That],
      convThat : That => GenTraversableLike[B, That]) = new Sized[That, sum.Out](r ++ that.unsized) { type A = B }
    
  def map[B, That](f : A => B)
    (implicit cbf : CanBuildFrom[Repr, B, That]) = new Sized[That, L](r map f) { type A = B }
}

trait LowPrioritySized {
  implicit def sizedToRepr[Repr](s : Sized[Repr, _]) : Repr = s.unsized
}

object Sized extends LowPrioritySized {
  import Nat._
  
  implicit def sizedOps[A0, Repr <% GenTraversableLike[A0, Repr], L <: Nat]
    (s : Sized[Repr, L] { type A = A0 }) : SizedOps[A0, Repr, L] = new SizedOps[A0, Repr, L](s.unsized)
  
  class SizedBuilder[CC[_]] {
    def apply[T](a : T)(implicit cbf : CanBuildFrom[Nothing, T, CC[T]]) = new Sized[CC[T], _1]((cbf() += a).result) { type A = T }

    def apply[T](a : T, b : T)
      (implicit cbf : CanBuildFrom[Nothing, T, CC[T]]) = new Sized[CC[T], _2]((cbf() += (a, b)).result) { type A = T }
        
    def apply[T](a : T, b : T, c : T)
      (implicit cbf : CanBuildFrom[Nothing, T, CC[T]]) = new Sized[CC[T], _3]((cbf() += (a, b, c)).result) { type A = T }
  }
  
  def apply[CC[_]] = new SizedBuilder[CC]
  
  def apply[CC[_]]()
    (implicit cbf : CanBuildFrom[Nothing, Nothing, CC[Nothing]]) =
      new Sized[CC[Nothing], _0](cbf().result) { type A = Nothing }

  class SizedConv[A0, Repr <% GenTraversableLike[A0, Repr]](r : Repr) {
    def sized[L <: Nat](implicit toInt : ToInt[L]) =
      if(r.size == toInt()) Some(new Sized[Repr, L](r) { type A = A0 }) else None
      
    def ensureSized[L <: Nat](implicit toInt : ToInt[L]) = {
      assert(r.size == toInt())
      new Sized[Repr, L](r) { type A = A0 }
    }
  }

  implicit def genTraversableSizedOps[CC[X] <: GenTraversable[X], T](cc : CC[T])
    (implicit conv : CC[T] => GenTraversableLike[T, CC[T]]) = new SizedConv[T, CC[T]](cc)
  
  implicit def stringSizedOps(s : String) = new SizedConv[Char, String](s)
}

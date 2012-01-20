package shapeless

import scala.collection.{ GenTraversable, GenTraversableOnce, GenTraversableLike }
import scala.collection.generic.CanBuildFrom
import scala.collection.immutable.WrappedString

abstract class Sized[Repr, L <: Nat](r : Repr) { outer =>
  import Sized._
  import Nat._
  import LT._
  
  type A
  implicit val conv : Repr => GenTraversableLike[A, Repr]  
  
  override def toString = r.toString
  def unsized = r
  
  def head(implicit ev : _0 < L) : A = r.head
  
  def tail(implicit pred : Pred[L]) = Sized[pred.Out](r.tail)
  
  def take[M <: Nat](implicit diff : Diff[L, M], ev : ToInt[M]) = Sized[M](r.take(toInt[M]))
  
  def drop[M <: Nat](implicit diff : Diff[L, M], ev : ToInt[M]) = Sized[diff.Out](r.drop(toInt[M]))
  
  def splitAt[M <: Nat](implicit diff : Diff[L, M], ev : ToInt[M]) = (take[M], drop[M])
  
  def +:(elem : A)(implicit cbf : CanBuildFrom[Repr, A, Repr]) = {
    val builder = cbf.apply(r)
    builder += elem
    builder ++= r.toIterator
    Sized[Succ[L]](builder.result)
  }
  
  def ++[B >: A, That <% GenTraversableLike[B, That], M <: Nat](that : SizedAux[B, That, M])
    (implicit
      sum : Sum[L, M],
      cbf : CanBuildFrom[Repr, B, That]
    ) = Sized[sum.Out](r ++ that.unsized)
}

class SizedOps[A, Repr <% GenTraversableLike[A, Repr]](r : Repr) {
  def sized[L <: Nat](implicit toInt : ToInt[L]) = if(r.size == toInt()) Some(Sized[L](r)) else None
}

object Sized {
  type SizedAux[A0, Repr, L <: Nat] = Sized[Repr, L] {
    type A = A0
  }
  
  class SizedBuilder[L <: Nat] {
    def apply[A0, Repr <% GenTraversableLike[A0, Repr]](r : Repr) = { 
      val conv0 = implicitly[Repr => GenTraversableLike[A0, Repr]]
      new Sized[Repr, L](r) {
        type A = A0
        implicit val conv = conv0
      }
    }
  }
  
  def apply[L <: Nat] = new SizedBuilder[L]
  
  implicit def genTraversableSizedOps[CC[X] <: GenTraversable[X], T](cc : CC[T])
    (implicit conv : CC[T] => GenTraversableLike[T, CC[T]]) = new SizedOps[T, CC[T]](cc)
  
  implicit def stringNListOps(s : String) = new SizedOps[Char, String](s)
  
  implicit def sizedToRepr[Repr](s : Sized[Repr, _]) : Repr = s.unsized
}

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
  
  def tail(implicit pred : Pred[L]) = wrap[pred.Out](r.tail)
  
  def take[M <: Nat](implicit diff : Diff[L, M], ev : ToInt[M]) = wrap[M](r.take(toInt[M]))
  
  def drop[M <: Nat](implicit diff : Diff[L, M], ev : ToInt[M]) = wrap[diff.Out](r.drop(toInt[M]))
  
  def splitAt[M <: Nat](implicit diff : Diff[L, M], ev : ToInt[M]) = (take[M], drop[M])
  
  def +:(elem : A)(implicit cbf : CanBuildFrom[Repr, A, Repr]) = {
    val builder = cbf.apply(r)
    builder += elem
    builder ++= r.toIterator
    wrap[Succ[L]](builder.result)
  }
  
  def ++[B >: A, That, M <: Nat](that : SizedAux[B, That, M])
    (implicit
      sum : Sum[L, M],
      cbf : CanBuildFrom[Repr, B, That]
    ) = {
    implicit val convThat = that.conv
    wrap[sum.Out](r ++ that.unsized)
  }
    
  def map[B, That](f : A => B)
    (implicit
      cbf : CanBuildFrom[Repr, B, That],
      convThat : That => GenTraversableLike[B, That]) = wrap[L](r map f)
}

object Sized {
  import Nat._
  
  class SizedBuilder[CC[_]] {
    def apply[T](a : T)
      (implicit
        cbf : CanBuildFrom[Nothing, T, CC[T]],
        conv : CC[T] => GenTraversableLike[T, CC[T]]) = wrap[_1]((cbf() += a).result)

    def apply[T](a : T, b : T)
      (implicit
        cbf : CanBuildFrom[Nothing, T, CC[T]],
        conv : CC[T] => GenTraversableLike[T, CC[T]]) = wrap[_2]((cbf() += (a, b)).result)
        
    def apply[T](a : T, b : T, c : T)
      (implicit
        cbf : CanBuildFrom[Nothing, T, CC[T]],
        conv : CC[T] => GenTraversableLike[T, CC[T]]) = wrap[_3]((cbf() += (a, b, c)).result)
  }
  
  def apply[CC[_]] = new SizedBuilder[CC]
  
  def apply[CC[_]]()
    (implicit
      cbf : CanBuildFrom[Nothing, Nothing, CC[Nothing]],
      conv : CC[Nothing] => GenTraversableLike[Nothing, CC[Nothing]]) = wrap[_0](cbf().result)

  type SizedAux[A0, Repr, L <: Nat] = Sized[Repr, L] {
    type A = A0
  }
  
  class SizedWrapper[L <: Nat] {
    def apply[A0, Repr <% GenTraversableLike[A0, Repr]](r : Repr) = { 
      val conv0 = implicitly[Repr => GenTraversableLike[A0, Repr]]
      new Sized[Repr, L](r) {
        type A = A0
        implicit val conv = conv0
      }
    }
  }
  
  private def wrap[L <: Nat] = new SizedWrapper[L]
  
  class SizedOps[A, Repr <% GenTraversableLike[A, Repr]](r : Repr) {
    def sized[L <: Nat](implicit toInt : ToInt[L]) = if(r.size == toInt()) Some(wrap[L](r)) else None
  }

  implicit def genTraversableSizedOps[CC[X] <: GenTraversable[X], T](cc : CC[T])
    (implicit conv : CC[T] => GenTraversableLike[T, CC[T]]) = new SizedOps[T, CC[T]](cc)
  
  implicit def stringSizedOps(s : String) = new SizedOps[Char, String](s)
  
  implicit def sizedToRepr[Repr](s : Sized[Repr, _]) : Repr = s.unsized
}

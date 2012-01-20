package shapeless

import scala.collection.{ GenTraversable, GenTraversableOnce, GenTraversableLike }
import scala.collection.generic.CanBuildFrom
import scala.collection.immutable.WrappedString

abstract class Sized[Repr, L <: Nat](r : Repr) { outer =>
  import Nat._
  import Pred._
  import LT._
  
  type A
  implicit val conv : Repr => GenTraversableLike[A, Repr]  
  
  override def toString = r.toString
  def unsized = r
  
  def head(implicit ev : _0 < L) : A = r.head
  
  def tail[M <: Nat](implicit pred : Pred[L, M]) = Sized[M](r.tail)
  
  def take[M <: Nat](implicit diff : DiffAux[L, M], ev : ToInt[M]) = Sized[M](r.take(toInt[M]))
  
  def drop[M <: Nat](implicit diff : DiffAux[L, M], ev : ToInt[M]) = Sized[diff.Out](r.drop(toInt[M]))
  
  def splitAt[M <: Nat](implicit diff : DiffAux[L, M], ev : ToInt[M]) = (take[M], drop[M])
  
  def +:(elem : A)(implicit cbf : CanBuildFrom[Repr, A, Repr]) : Sized[Repr, Succ[L]] = {
    val builder = cbf.apply(r)
    builder += elem
    builder ++= r.toIterator
    Sized[Succ[L]](builder.result)
  }
  
  def ++[B >: A, That <% GenTraversableLike[B, That], M <: Nat, N <: Nat](that : Sized[That, M] { type A = B })
    (implicit
      ev : Sum[L, M, N],
      cbf : CanBuildFrom[Repr, B, That]
    ) = Sized[N](r ++ that.unsized)
}

class SizedOps[A0, Repr <% GenTraversableLike[A0, Repr]](r : Repr) {
  import Nat._
  def sized[L <: Nat](implicit ev : ToInt[L]) : Option[Sized[Repr, L] { type A = A0 }] =
    if(r.size == toInt[L]) Some(Sized[L](r)) else None
}

object Sized {
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

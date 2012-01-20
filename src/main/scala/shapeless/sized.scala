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
  
  def tail[M <: Nat](implicit pred : Pred[L, M]) = new Sized[Repr, M](r.tail) {
    type A = outer.A
    implicit val conv = outer.conv
  }
  
  def take[M <: Nat](implicit diff : DiffAux[L, M], ev : ToInt[M]) = new Sized[Repr, M](r.take(toInt[M])) {
    type A = outer.A
    implicit val conv = outer.conv
  }
  
  def drop[M <: Nat](implicit diff : DiffAux[L, M], ev : ToInt[M]) = new Sized[Repr, diff.Out](r.drop(toInt[M])) {
    type A = outer.A
    implicit val conv = outer.conv
  }
  
  def splitAt[M <: Nat](implicit diff : DiffAux[L, M], ev : ToInt[M]) = {
    val prefix = new Sized[Repr, M](r.take(toInt[M])) {
      type A = outer.A
      implicit val conv = outer.conv
    }
    val suffix = new Sized[Repr, diff.Out](r.drop(toInt[M])) {
      type A = outer.A
      implicit val conv = outer.conv
    }
    
    (prefix, suffix)
  }
  
  def +:(elem : A)(implicit cbf : CanBuildFrom[Repr, A, Repr]) : Sized[Repr, Succ[L]] = {
    val builder = cbf.apply(r)
    builder += elem
    builder ++= r.toIterator
    new Sized[Repr, Succ[L]](builder.result) {
      type A = outer.A
      implicit val conv = outer.conv
    }
  }
  
  def ++[B >: A, That <% GenTraversableLike[B, That], M <: Nat, N <: Nat](that : Sized[That, M] { type A = B })
    (implicit
      ev : Sum[L, M, N],
      cbf : CanBuildFrom[Repr, B, That]
    ) = new Sized[That, N](r ++ that.unsized) {
    type A = that.A
    implicit val conv = that.conv
  }
}

class SizedOps[A0, Repr <% GenTraversableLike[A0, Repr]](r : Repr) {
  import Nat._
  def sized[L <: Nat](implicit ev : ToInt[L]) : Option[Sized[Repr, L] { type A = A0 }] = {
    if(r.size == toInt[L]) {
      val conv0 = implicitly[Repr => GenTraversableLike[A0, Repr]]
      val nl = new Sized[Repr, L](r) {
        type A = A0
        implicit val conv = conv0
      }
      Some(nl)
    }
    else None
  }
}

object Sized {
  implicit def genTraversableSizedOps[CC[X] <: GenTraversable[X], T](cc : CC[T])
    (implicit conv : CC[T] => GenTraversableLike[T, CC[T]]) = new SizedOps[T, CC[T]](cc)
  
  implicit def stringNListOps(s : String) = new SizedOps[Char, String](s)
  
  implicit def sizedToRepr[Repr](s : Sized[Repr, _]) : Repr = s.unsized
}

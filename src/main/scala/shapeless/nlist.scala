package shapeless

import scala.collection.{ GenTraversable, GenTraversableOnce, GenTraversableLike }
import scala.collection.generic.CanBuildFrom
import scala.collection.immutable.WrappedString

abstract class NList[Repr, L <: Nat](r : Repr) { outer =>
  import Nat._
  import Pred._
  import LT._
  
  type A
  implicit val conv : Repr => GenTraversableLike[A, Repr]  
  
  override def toString = r.toString
  def toRepr = r
  
  def head(implicit ev : _0 < L) : A = r.head 
  def tail[M <: Nat](implicit pred : Pred[L, M]) = new NList[Repr, M](r.tail) {
    type A = outer.A
    implicit val conv = outer.conv
  }
  
  def +:(elem : A)(implicit cbf : CanBuildFrom[Repr, A, Repr]) : NList[Repr, Succ[L]] = {
    val builder = cbf.apply(r)
    builder += elem
    builder ++= r.toIterator
    new NList[Repr, Succ[L]](builder.result) {
      type A = outer.A
      implicit val conv = outer.conv
    }
  }
  
  def ++[B >: A, That <% GenTraversableLike[B, That], M <: Nat, N <: Nat](that : NList[That, M] { type A = B })
    (implicit
      ev : Sum[L, M, N],
      cbf : CanBuildFrom[Repr, B, That]
    ) = new NList[That, N](r ++ that.toRepr) {
    type A = that.A
    implicit val conv = that.conv
  }
}

class NListOps[A0, Repr <% GenTraversableLike[A0, Repr]](r : Repr) {
  import Nat._
  def toNList[L <: Nat](implicit ev : ToInt[L]) : Option[NList[Repr, L] { type A = A0 }] = {
    if(r.size == toInt[L]) {
      val conv0 = implicitly[Repr => GenTraversableLike[A0, Repr]]
      val nl = new NList[Repr, L](r) {
        type A = A0
        implicit val conv = conv0
      }
      Some(nl)
    }
    else None
  }
}

object NList {
  
  implicit def genTraversableNListOps[CC[X] <: GenTraversable[X], T](cc : CC[T])
    (implicit conv : CC[T] => GenTraversableLike[T, CC[T]]) = new NListOps[T, CC[T]](cc)
  
  implicit def stringNListOps(s : String) = new NListOps[Char, String](s)
}

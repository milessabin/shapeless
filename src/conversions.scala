object Tuples {
  import HList._
  
  object Implicits {
    implicit def hlistToTuple1[A](l : A :: HNil) = Tuple1(l.head)
    implicit def hlistToTuple2[A, B](l : A :: B :: HNil) = (l.head, l.tail.head)
    implicit def hlistToTuple3[A, B, C](l : A :: B :: C :: HNil) = (l.head, l.tail.head, l.tail.tail.head)
    implicit def hlistToTuple4[A, B, C, D](l : A :: B :: C :: D :: HNil) = (l.head, l.tail.head, l.tail.tail.head, l.tail.tail.tail.head)
    
    implicit def tuple1ToHList[A](t : Product1[A]) = t._1 :: HNil
    implicit def tuple2ToHList[A, B](t : Product2[A, B]) = t._1 :: t._2 :: HNil
    implicit def tuple3ToHList[A, B, C](t : Product3[A, B, C]) = t._1 :: t._2 :: t._3 :: HNil
    implicit def tuple4ToHList[A, B, C, D](t : Product4[A, B, C, D]) = t._1 :: t._2 :: t._3 :: t._4 :: HNil
  }

  import Implicits._
  
  trait TupleOps[L <: HList] {
    def hlisted : L
  }
  
  implicit def tuple1Ops[A](t : Product1[A]) = new TupleOps[A :: HNil] { def hlisted = t } 
  implicit def tuple2Ops[A, B](t : Product2[A, B]) = new TupleOps[A :: B ::HNil] { def hlisted = t }
  implicit def tuple3Ops[A, B, C](t : Product3[A, B, C]) = new TupleOps[A :: B :: C :: HNil] { def hlisted = t }
  implicit def tuple4Ops[A, B, C, D](t : Product4[A, B, C, D]) = new TupleOps[A :: B :: C :: D :: HNil] { def hlisted = t }
  
  trait HListOps[T] {
    def tupled : T
  }
  
  implicit def hlist1Ops[A](l : A :: HNil) = new HListOps[Tuple1[A]] { def tupled = l }
  implicit def hlist2Ops[A, B](l : A :: B :: HNil) = new HListOps[(A, B)] { def tupled = l }
  implicit def hlist3Ops[A, B, C](l : A :: B :: C :: HNil) = new HListOps[(A, B, C)] { def tupled = l }
  implicit def hlist4Ops[A, B, C, D](l : A :: B :: C :: D :: HNil) = new HListOps[(A, B, C, D)] { def tupled = l }
}

object Functions {
  import PolyFun._
  import HList._
  import Tuples._
  
  object Implicits {
    implicit def hlistFnToFn1[A, T](hf : A :: HNil => T) = (a : A) => hf(a :: HNil)
    implicit def hlistFnToFn2[A, B, T](hf : A :: B :: HNil => T) = (a : A, b : B) => hf(a :: b :: HNil)
    implicit def hlistFnToFn3[A, B, C, T](hf : A :: B :: C :: HNil => T) = (a : A, b : B, c : C) => hf(a :: b :: c :: HNil)
    implicit def hlistFnToFn4[A, B, C, D, T](hf : A :: B :: C :: D :: HNil => T) = (a : A, b : B, c : C, d : D) => hf(a :: b :: c :: d :: HNil)
    
    implicit def fn1ToHListFn[A, T](f : A => T) = (l : A :: HNil) => f(l.head)
    implicit def fn2ToHListFn[A, B, T](f : (A, B) => T) = (l : A :: B :: HNil) => f.tupled(l.tupled)
    implicit def fn3ToHListFn[A, B, C, T](f : (A, B, C) => T) = (l : A :: B :: C :: HNil) => f.tupled(l.tupled)
    implicit def fn4ToHListFn[A, B, C, D, T](f : (A, B, C, D) => T) = (l : A :: B :: C :: D :: HNil) => f.tupled(l.tupled)
  }
  
  import Implicits._
  
  trait FnHListOps[L <: HList, T] {
    def hlisted : L => T
  }
  
  implicit def fn1Ops[A, T](t : A => T) = new FnHListOps[A :: HNil, T] { def hlisted = t } 
  implicit def fn2Ops[A, B, T](t : (A, B) => T) = new FnHListOps[A :: B ::HNil, T] { def hlisted = t }
  implicit def fn3Ops[A, B, C, T](t : (A, B, C) => T) = new FnHListOps[A :: B :: C :: HNil, T] { def hlisted = t }
  implicit def fn4Ops[A, B, C, D, T](t : (A, B, C, D) => T) = new FnHListOps[A :: B :: C :: D :: HNil, T] { def hlisted = t }

  trait HListFnOps[F] {
    def unhlisted : F
  }
  
  implicit def hlistFn1Ops[A, T](hf : A :: HNil => T) = new HListFnOps[A => T] { def unhlisted = hf }
  implicit def hlistFn2Ops[A, B, T](hf : A :: B :: HNil => T) = new HListFnOps[(A, B) => T] { def unhlisted = hf }
  implicit def hlistFn3Ops[A, B, C, T](hf : A :: B :: C :: HNil => T) = new HListFnOps[(A, B, C) => T] { def unhlisted = hf }
  implicit def hlistFn4Ops[A, B, C, D, T](hf : A :: B :: C :: D ::HNil => T) = new HListFnOps[(A, B, C, D) => T] { def unhlisted = hf }
}

object Traversables {
  import scala.collection.Traversable
  
  import HList._
  import Castable._

  trait FromTraversable[T, Out <: HList] {
    def apply(l : Traversable[T]) : Option[Out]
  }
  
  implicit def hnilFromTraversable[T] = new FromTraversable[T, HNil] {
    def apply(l : Traversable[T]) = l match {
      case Nil => Some(HNil)
      case _ => None
    }
  }
  
  implicit def hlistFromTraversable[T, OutH : Castable, OutT <: HList](implicit flt : FromTraversable[T, OutT]) = new FromTraversable[T, OutH :: OutT] {
    def apply(l : Traversable[T]) : Option[OutH :: OutT] = for(e <- l.headOption; h <- e.cast[OutH]; t <- flt(l.tail)) yield h :: t
  }
  
  trait TraversableOps[T] {
    def toHList[L <: HList](implicit fl : FromTraversable[T, L]) : Option[L]
  }
  
  implicit def traversableOps[T](l : Traversable[T]) = new TraversableOps[T] {
    def toHList[L <: HList](implicit fl : FromTraversable[T, L]) = fl(l) 
  }
}

object TestTuples {
  import Tuples._
  import Implicits._
  
  def main(arg : Array[String]) {
    case class Foo(a : Int, b : String, c : Double)
    
    val f1 = Foo(23, "foo", 2.3)
    println(f1)
    val hf = f1.hlisted
    val f2 = Foo.tupled(hf)
    println(f2)
    
    println(f1 == f2)
  }
}

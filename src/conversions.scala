object Tuples {
  import Rank2Poly._
  import HLists._
  

  implicit def tuple1ToHList[A](t : Product1[A]) = t._1 :: HNil[Id]
  implicit def tuple2ToHList[A, B](t : Product2[A, B]) = t._1 :: t._2 :: HNil[Id]
  implicit def tuple3ToHList[A, B, C](t : Product3[A, B, C]) = t._1 :: t._2 :: t._3 :: HNil[Id]
  implicit def tuple4ToHList[A, B, C, D](t : Product4[A, B, C, D]) = t._1 :: t._2 :: t._3 :: t._4 :: HNil[Id]

  trait TupleOps[L <: HList[Id]] {
    def hlisted : L
  }
  
  implicit def tuple1Ops[A](t : Product1[A]) = new TupleOps[A :: HNil[Id]] { def hlisted = t._1 :: HNil[Id] } 
  implicit def tuple2Ops[A, B](t : Product2[A, B]) = new TupleOps[A :: B ::HNil[Id]] { def hlisted = t._1 :: t._2 :: HNil[Id] }
  implicit def tuple3Ops[A, B, C](t : Product3[A, B, C]) = new TupleOps[A :: B :: C :: HNil[Id]] { def hlisted = t._1 :: t._2 :: t._3 :: HNil[Id] }
  implicit def tuple4Ops[A, B, C, D](t : Product4[A, B, C, D]) = new TupleOps[A :: B :: C :: D :: HNil[Id]] { def hlisted = t._1 :: t._2 :: t._3 :: t._4 :: HNil[Id] }
}

object Functions {
  import Rank2Poly._
  import HLists._
  import Tuples._
  
  abstract class HListFn[F[_], T <: HList[F], R](val f : T#Fn[R]) extends (T => R) {
    type Tupled = T#Tupled => R
    type Fn = T#Fn[R]
  }
  implicit def fnToHListFn1[A, R](f : A => R) = new HListFn[Id, A :: HNil[Id], R](f) {
    def apply(h : A :: HNil[Id]) = f(h.head)
  }
  implicit def fnToHListFn2[A, B, R](f : (A, B) => R) = new HListFn[Id, A :: B :: HNil[Id], R](f) {
    def apply(h : A :: B :: HNil[Id]) = f.tupled(h.tupled)
  }
  implicit def fnToHListFn3[A, B, C, R](f : (A, B, C) => R) = new HListFn[Id, A :: B :: C :: HNil[Id], R](f) {
    def apply(h : A :: B :: C :: HNil[Id]) = f.tupled(h.tupled)
  }
  implicit def fnToHListFn4[A, B, C, D, R](f : (A, B, C, D) => R) = new HListFn[Id, A :: B :: C :: D :: HNil[Id], R](f) {
    def apply(h : A :: B :: C :: D :: HNil[Id]) = f.tupled(h.tupled)
  }
  
  implicit def hlistFnToFn1[A, T](hf : A :: HNil[Id] => T) = (a : A) => hf(a :: HNil[Id])
  implicit def hlistFnToFn2[A, B, T](hf : A :: B :: HNil[Id] => T) = Function.untupled((t : (A, B)) => hf(t)) 
  implicit def hlistFnToFn3[A, B, C, T](hf : A :: B :: C :: HNil[Id] => T) = Function.untupled((t : (A, B, C)) => hf(t))
  implicit def hlistFnToFn4[A, B, C, D, T](hf : A :: B :: C :: D :: HNil[Id] => T) = Function.untupled((t : (A, B, C, D)) => hf(t))
}

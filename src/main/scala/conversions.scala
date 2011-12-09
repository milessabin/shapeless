object Tuples {
  import HList._
  import PolyFun._
  
  trait HLister[-T <: Product] {
    type Out <: HList
    def apply(t : T) : Out
  }
  
  implicit def hlister[T <: Product, Out0 <: HList](implicit hlister : HLister0[T, Out0]) = new HLister[T] {
    type Out = Out0
    def apply(t : T) : Out = hlister(t)
  }
  
  type HListerAux[-T <: Product, Out <: HList] = HLister0[T, Out]
  
  trait HLister0[-T <: Product, Out <: HList] {
    def apply(t : T) : Out
  }
  
  implicit def tupleHLister1[A] = new HLister0[Product1[A], A :: HNil] {
    def apply(t : Product1[A]) = t._1 :: HNil
  }
  
  implicit def tupleHLister2[A, B] = new HLister0[Product2[A, B], A :: B :: HNil] {
    def apply(t : Product2[A, B]) = t._1 :: t._2 :: HNil
  }
  
  implicit def tupleHLister3[A, B, C] = new HLister0[Product3[A, B, C], A :: B :: C :: HNil] {
    def apply(t : Product3[A, B, C]) = t._1 :: t._2 :: t._3 :: HNil
  }

  implicit def tupleHLister4[A, B, C, D] = new HLister0[Product4[A, B, C, D], A :: B :: C :: D :: HNil] {
    def apply(t : Product4[A, B, C, D]) = t._1 :: t._2 :: t._3 :: t._4 :: HNil
  }
  
  trait TupleOps[L <: HList] {
    def hlisted : L
  }
  
  implicit def tupleOps[T <: Product](t : T)(implicit hlister : HLister[T]) = new TupleOps[hlister.Out] {
    def hlisted = hlister(t)
  }
  
  object hlisted {
    def apply[T <: Product](t : T)(implicit hlister : HLister[T]) : hlister.Out = hlister(t)
  }
  implicit def hlisted1[T <: Product](implicit hlister : HLister[T]) = new Case[hlisted.type, T => hlister.Out](hlister.apply(_))

  implicit def univInstHListed[F, G](h : hlisted.type)(implicit c : Case[hlisted.type, F => G]) : F => G = c.value
  
  object tupled {
    def apply[L <: HList](l : L)(implicit tupler : Tupler[L]) : tupler.Out = tupler(l)
  }
  implicit def tupled1[L <: HList](implicit tupler : Tupler[L]) = new Case[tupled.type, L => tupler.Out](tupler.apply(_))
  
  implicit def univInstTupled[F, G](t : tupled.type)(implicit c : Case[tupled.type, F => G]) : F => G = c.value
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
  import Typeable._

  trait FromTraversable[T, Out <: HList] {
    def apply(l : Traversable[T]) : Option[Out]
  }
  
  implicit def hnilFromTraversable[T] = new FromTraversable[T, HNil] {
    def apply(l : Traversable[T]) = l match {
      case Nil => Some(HNil)
      case _ => None
    }
  }
  
  implicit def hlistFromTraversable[T, OutH, OutT <: HList](implicit flt : FromTraversable[T, OutT], oc : Typeable[OutH]) = new FromTraversable[T, OutH :: OutT] {
    def apply(l : Traversable[T]) : Option[OutH :: OutT] = for(e <- l.headOption; h <- e.cast[OutH]; t <- flt(l.tail)) yield h :: t
  }
  
  trait TraversableOps[T] {
    def toHList[L <: HList](implicit fl : FromTraversable[T, L]) : Option[L]
  }
  
  implicit def traversableOps[T](l : Traversable[T]) = new TraversableOps[T] {
    def toHList[L <: HList](implicit fl : FromTraversable[T, L]) = fl(l) 
  }
}

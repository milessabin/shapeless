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
  
  trait FnHLister[F] {
    type Out
    def apply(f : F) : Out
  }
  
  implicit def fnHLister[F, Out0](implicit fnHLister : FnHLister0[F, Out0]) = new FnHLister[F] {
    type Out = Out0
    def apply(f : F) : Out = fnHLister(f)
  }
  
  type FnHListerAux[F, Out] = FnHLister0[F, Out] 
  
  trait FnHLister0[F, Out] {
    def apply(f : F) : Out
  }
  
  implicit def fnHLister1[A, R] = new FnHLister0[A => R, (A :: HNil) => R] {
    def apply(f : A => R) = (l : A :: HNil) => f(l.head)
  }
  
  implicit def fnHLister2[A, B, R] = new FnHLister0[(A, B) => R, (A :: B :: HNil) => R] {
    def apply(f : (A, B) => R) = (l : A :: B :: HNil) => f(l.head, l.tail.head)
  }
  
  implicit def fnHLister3[A, B, C, R] = new FnHLister0[(A, B, C) => R, (A :: B :: C :: HNil) => R] {
    def apply(f : (A, B, C) => R) = (l : A :: B :: C :: HNil) => f(l.head, l.tail.head, l.tail.tail.head)
  }
  
  implicit def fnHLister4[A, B, C, D, R] = new FnHLister0[(A, B, C, D) => R, (A :: B :: C :: D :: HNil) => R] {
    def apply(f : (A, B, C, D) => R) = (l : A :: B :: C :: D :: HNil) => f(l.head, l.tail.head, l.tail.tail.head, l.tail.tail.tail.head)
  }
  
  trait FnHListOps[HLFn] {
    def hlisted : HLFn
  }
  
  implicit def fnHListOps[F](t : F)(implicit fnHLister : FnHLister[F]) = new FnHListOps[fnHLister.Out] {
    def hlisted = fnHLister(t)
  }

  trait FnUnHLister[F] {
    type Out
    def apply(f : F) : Out
  }
  
  implicit def fnUnHLister[F, Out0](implicit fnUnHLister : FnUnHLister0[F, Out0]) = new FnHLister[F] {
    type Out = Out0
    def apply(f : F) : Out = fnUnHLister(f)
  }
  
  type FnUnHListerAux[F, Out] = FnUnHLister0[F, Out] 
  
  trait FnUnHLister0[F, Out] {
    def apply(f : F) : Out
  }
  
  implicit def fnUnHLister1[A, R] = new FnUnHLister0[(A :: HNil) => R, A => R] {
    def apply(f : (A :: HNil) => R) = (a : A) => f(a :: HNil)
  }
  
  implicit def fnUnHLister2[A, B, R] = new FnUnHLister0[(A :: B :: HNil) => R, (A, B) => R] {
    def apply(f : (A :: B :: HNil) => R) = (a : A, b : B) => f(a :: b :: HNil)
  }

  implicit def fnUnHLister3[A, B, C, R] = new FnUnHLister0[(A :: B :: C :: HNil) => R, (A, B, C) => R] {
    def apply(f : (A :: B :: C :: HNil) => R) = (a : A, b : B, c : C) => f(a :: b :: c :: HNil)
  }

  implicit def fnUnHLister4[A, B, C, D, R] = new FnUnHLister0[(A :: B :: C :: D :: HNil) => R, (A, B, C, D) => R] {
    def apply(f : (A :: B :: C :: D :: HNil) => R) = (a : A, b : B, c : C, d : D) => f(a :: b :: c :: d :: HNil)
  }
  
  trait FnUnHListOps[F] {
    def unhlisted : F
  }

  implicit def fnUnHListOps[F](t : F)(implicit fnUnHLister : FnUnHLister[F]) = new FnUnHListOps[fnUnHLister.Out] {
    def unhlisted = fnUnHLister(t)
  }
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

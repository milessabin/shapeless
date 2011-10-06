object MapFn {
  import Rank2Poly._
  import HLists._  

  type Trans = {
    type λ[T, R] <: (T => R)
  }
  
  trait TransCase[F, T, R] extends (T => R) {
    val f : T => R
    def apply(t : T) : R = f(t)
  }

  trait TransDef[F0[_], G0[_]] {
    trait Trans {
      type λ[T, R] = TransCase[Trans, T, R]
    }
    
    def apply[T](f0 : F0[T] => G0[T]) = new TransCase[Trans, F0[T], G0[T]] { val f = f0 } 
  }
  
  object Choose extends TransDef[Set, Option]
  type Choose = Choose.Trans
  
  implicit def chooseDflt[X] = Choose[X](choose)

  
  trait Mapper[T <: Trans, In, Out] {
    def map(t : In) : Out
  }
  
  implicit def hnilMapper[T <: Trans] = new Mapper[T, HNil, HNil] {
    def map(l : HNil) = HNil
  }
  
  implicit def hlistMapper[T <: Trans, InH, OutH, InT <: HList, OutT <: HList](implicit fh : T#λ[InH, OutH], mt : Mapper[T, InT, OutT]) = new Mapper[T, InH :: InT, OutH :: OutT] {
    def map(l : InH :: InT) = HCons(fh(l.head), mt.map(l.tail))
  }

  trait PartialMap[T <: Trans] {
    def apply[In <: HList, Out <: HList](in : In)(implicit mapper : Mapper[T, In, Out]) : Out = mapper.map(in)
  }
  
  def map[T <: Trans] = new PartialMap[T] {}
}

object TestMapFn {
  import HLists._
  import MapFn._

  def main(args : Array[String]) {
    type SISS = Set[Int] :: Set[String] :: HNil
    type OIOS = Option[Int] :: Option[String] :: HNil
    
    val l1 = Set(1) :: Set("foo") :: HNil
    val l2 : OIOS = map[Choose](l1)
    
    println(l1)
    println(l2)
  }
}

object MapFn {
  import Rank2Poly._
  import HLists._  

  type Trans = {
    type λ[T, R] <: (T => R)
    type F[_]
    type G[_]
  }
  
  trait TransCase[F, T, R] extends (T => R) {
    val f : T => R
    def apply(t : T) : R = f(t)
  }

  trait TransDef[F0[_], G0[_]] {
    trait Trans {
      type λ[T, R] = TransCase[Trans, T, R]
      type F[X] = F0[X]
      type G[X] = G0[X]
    }
    
    def apply[T](f0 : F0[T] => G0[T]) = new TransCase[Trans, F0[T], G0[T]] { val f = f0 } 
  }
  
  object Get extends TransDef[Option, Id]
  type Get = Get.Trans
  
  implicit def getDflt[X] = Get[X](get)

  
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
    type OIOS = Option[Int] :: Option[String] :: HNil
    type IS = Int :: String :: HNil
    
    val l1 = Option(1) :: Option("foo") :: HNil
    val l2 : IS = map[Get](l1)
    
    println(l1)
    println(l2)
  }
}

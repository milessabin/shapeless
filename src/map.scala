object MapFn {
  import HLists._  
  import Rank2Poly._

  trait Applicator[H, T, R] {
    def apply(t : T) : R
  }

  implicit def applicator[F[_], G[_], H <: F ~> G, T](f : H) : Applicator[H, F[T], G[T]] = new Applicator[H, F[T], G[T]] {
    def apply(t : F[T]) : G[T] = f(t)
  }

  def siapp[T] = implicitly[choose.type => Applicator[choose.type, Set[T], Option[T]]]
  val oi : Option[Int] = siapp(choose)(Set(23))
  val os : Option[String] = siapp(choose)(Set("foo"))

  trait Mapper[H, -In, +Out] {
    def map(f : H)(t : In) : Out
  }
  
  implicit def hnilMapper[H] = new Mapper[H, HNil, HNil] {
    def map(f : H)(l : HNil) = HNil
  }
  
  implicit def hlistMapper[H, InH, OutH, InT <: HList, OutT <: HList](implicit ap : H => Applicator[H, InH, OutH], mt : Mapper[H, InT, OutT]) = new Mapper[H, InH :: InT, OutH :: OutT] {
    def map(f : H)(l : InH :: InT) = HCons(ap(f)(l.head), mt.map(f)(l.tail))
  }

  def map[H, In <: HList, Out <: HList](f : H)(in : In)(implicit mapper : Mapper[H, In, Out]) : Out = mapper.map(f)(in)
}


object TestMapFn {
  import HLists._
  import MapFn._
  import Rank2Poly._

  def main(args : Array[String]) {
    type SI = Set[Int] :: HNil
    type OI = Option[Int] :: HNil

    type SISS = Set[Int] :: Set[String] :: HNil
    type OIOS = Option[Int] :: Option[String] :: HNil
    
    val s1 = Set(1) :: HNil
    val o1 : OI = map(choose)(s1)

    println(s1)
    println(o1)

    val s2 = Set(1) :: Set("foo") :: HNil
    val o2 : OIOS = map(choose)(s2)
    
    println(s2)
    println(o2)
  }
}

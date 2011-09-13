/*
object MapFn {
  import Rank2Poly._
  import HLists._  

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

  trait PartialMapTuple[T <: Trans] {
    def apply[In <: HList, Out <: HList, X](t : X)(implicit ev1 : X => In, mapper : Mapper[T, In, Out]) : Out#Tupled = mapper.map(t).tupled
  }
  
  def mapTuple[T <: Trans] = new PartialMapTuple[T] {}
}

object TestMapFn {
  import IncFn._
  import GetFn._
  import IsDefinedFn._
  import HLists._
  import MapFn._
  import Tuples._

  def main(args : Array[String]) {
    type ISII = Int :: String :: Int :: Int :: HNil 
    
    val l1 : ISII = 1 :: "foo" :: 2 :: 3 :: HNil
    println(l1)

    val l2 : ISII = map[Inc](l1)
    println(l2)
    
    type OIOS = Option[Int] :: Option[String] :: HNil
    type IS = Int :: String :: HNil
    
    val l3 = Option(1) :: Option("foo") :: HNil
    val l4 : IS = map[Get](l3)
    
    println(l3)
    println(l4)
    
    val l5 : IS = map[Get](l3)
    println(l5)

    type OIODOBOSOI = Option[Int] :: Option[Double] :: Option[Boolean] :: Option[String] :: Option[Int] :: HNil
    type IDBSI = Int :: Double :: Boolean :: String :: Int :: HNil
    type BBBBB = Boolean :: Boolean :: Boolean :: Boolean :: Boolean :: HNil
    
    val l6 : OIODOBOSOI = Option(1) :: Option(1.0) :: Option(false) :: Option("foo") :: Option(2) :: HNil
    val l7 : OIODOBOSOI = Option(1) :: Option(1.0) :: (None : Option[Boolean]) :: Option("foo") :: Option(2) :: HNil

    val l8 : IDBSI = map[Get](l6)
    println(l8)

    val l9 : BBBBB = map[IsDefined](l6)
    println(l9)
    
    val l10 : BBBBB = map[IsDefined](l7)
    println(l10)

    val t1 = (1, "foo", 2)
    val t2 = mapTuple[Inc](t1)
    val t3 : (Int, String, Int) = t2
  }
}
*/
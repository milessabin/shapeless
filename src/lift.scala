object LiftOFn {
  import PolyFun._
  import HList._
  import Tuples._
  import Functions._

  def liftO[In <: HList, Out <: HList, R](f :  In => R)
    (implicit m : Mapper[Option ~> Id, Out, In], lf : LeftFolder[Out, Boolean, Option]) : Out => Option[R] = 
      (o : Out) => if(lf(o, true, isDefined, _ && _)) Some(f(m(get, o))) else None 
}

object TestLiftOFn {
  import PolyFun._
  import HList._
  import Tuples._
  import Functions._
  import LiftOFn._
  
  def main(args : Array[String]) {
    
    val sum : (Int, Int) => Int = _ + _
    val prd : (Int, Int, Int) => Int = _ * _ * _
    
    val hlsum = sum.hlisted
    val hlprd = prd.hlisted
    
    val l1 = 2 :: 3 :: HNil
    val l2 = 2 :: 3 :: 4 :: HNil
    
    val s1 = hlsum(l1)
    println(s1)
    
    val p1 = hlprd(l2)
    println(p1)
    
    val l3 = Option(2) :: Option(3) :: HNil
    if (l3.foldLeft(true)(isDefined)(_ & _)) {
      val l3a = l3 map get
      val s2 = hlsum(l3a)
      println(s2)
    }
    val l4 = Option(2) :: Option(3) :: Option(4) :: HNil
    if (l4.foldLeft(true)(isDefined)(_ & _)) {
      val l4a = l4 map get
      val p2 = hlprd(l4a)
      println(p2)
    }
    
    import Functions.Implicits._
    
    val sumO = liftO(sum)

    val s2 = sumO(Some(1), Some(2))
    println(s2)

    val s3 = sumO(Some(1), None)
    println(s3)
    
    val s4 = sumO(None, Some(2))
    println(s4)
    
    val s5 = sumO(None, None)
    println(s5)
    
    val s6 = List(Some(1), Some(2), Some(3), Some(4)).reduce(sumO)
    println(s6)
    
    val prdO = liftO(prd)

    val p2 = prdO(Some(2), Some(3), Some(4))
    println(p2)

    val p3 = prdO(Some(2), None, Some(4))
    println(p3)

    val p4 = prdO(Some(2), Some(3), None)
    println(p4)
  }
}

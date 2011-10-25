object LiftOFn {
  import PolyFun._
  import HLists._
  import Tuples._
  import Functions._
  
  def liftO[H, T <: HList, R](f : (H :: T) => R)
    (implicit mapper : Mapper[Option, Id, (H :: T)#Mapped[Option], H :: T], folder : LeftFolder[(H :: T)#Mapped[Option], Boolean, Option]) =
      (ol : (H :: T)#Mapped[Option]) => if (ol.foldLeft(true)(isDefined)(_ && _)) Some(f(ol map get)) else None
}

object TestLiftOFn {
  import PolyFun._
  import HLists._
  import Tuples._
  import Functions._
  import LiftOFn._
  
  def main(args : Array[String]) {
    
    val sum : (Int, Int) => Int = _ + _
    val prd : (Int, Int, Int) => Int = _ * _ * _
    
    val l1 = 2 :: 3 :: HNil
    val l2 = 2 :: 3 :: 4 :: HNil
    
//    val sumO = liftO(sum.hlisted)
//    val prdO = liftO(prd.hlisted)
/*
    import Functions.Implicits._
    val sumO = liftO(sum)
    val prdO = liftO(prd)

    val s2 = sumO(Some(1), Some(2))
    println(s2)

    val s3 = sumO(Some(1), None)
    println(s3)
    
    val s4 = sumO(None, Some(2))
    println(s4)
    
    val s5 = sumO(None, None)
    println(s5)
    
    val p2 = prdO(Some(2), Some(3), Some(4))
    println(p2)

    val p3 = prdO(Some(2), None, Some(4))
    println(p3)
  */
  }
}

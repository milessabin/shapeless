object LiftOFn {
  import Rank2Poly._
  import HLists._
  import Tuples._
  import Functions._
  
  def liftO[H <: HList[Id], R, HF <: HListFn[Id, H, R]](f : HF) : f.Fn = null.asInstanceOf[f.Fn]
}

object TestLiftOFn {
  import Functions._
  import LiftOFn._
  
  def main(args : Array[String]) {
    
    val sum = fnToHListFn2((_ : Int) + (_ : Int))
    val prod = fnToHListFn3((_ : Int) * (_ : Int) * (_ : Int))
    
    //val sumOO = liftO(sum)
    //val prodOO = liftO(prod)

    /*
    val sumO = hlistFnToFn2(sumOO)
    val prodO = hlistFnToFn3(prodOO)
  
    val s1 = sumO(Some(1), Some(2))
    println(s1)

    val s2 = sumO(Some(1), None)
    println(s2)
    
    val s3 = sumO(None, Some(2))
    println(s3)
    
    val s4 = sumO(None, None)
    println(s4)
    
    val p1 = prodO(Some("foo"), Some(3), Some(4))
    println(p1)

    val p2 = prodO(Some(2), None, Some(4))
    println(p2)
    */
  }
}

object LiftOFn {
  import Rank2Poly._
  import HLists._
  import Tuples._
  import Functions._
  
  def liftO[T <: HList[Id], R](f : HListFn[Id, T, R]) : HListFn[Option, T#Mapped[Option], Option[R]] = {
    new HListFn[Option, T#Mapped[Option], Option[R]] {
      def apply(ot : T#Mapped[Option]) : Option[R] = {
        if (ot.foldLeft(isDefined)(true)(_ & _)) {
          val t = (ot map get).asInstanceOf[T]
          Some(f(t))
        }
        else None
      }
    }
  }
}

object TestLiftOFn {
  import Rank2Poly._
  import HLists._
  import Tuples._
  import Functions._
  import LiftOFn._
  
  def main(args : Array[String]) {
    
    val sum : (Int, Int) => Int = _ + _
    val prd : (Int, Int, Int) => Int = _ * _ * _
    
    val hlsum = fnToHListFn2(sum)
    val hlprd = fnToHListFn3(prd)
    
    val l1 = 2 :: 3 :: HNil[Id]
    val l2 = 2 :: 3 :: 4 :: HNil[Id]
    
    val s1 = hlsum(l1)
    println(s1)
    
    val p1 = hlprd(l2)
    println(p1)
    
    val l3 : (Int :: Int :: HNil[Id])#Mapped[Option] = Option(2) :: Option(3) :: HNil[Option]
    if (l3.foldLeft(isDefined)(true)(_ & _)) {
      val l3a = l3 map get
      val s2 = hlsum(l3a)
      println(s2)
    }
    val l4 : (Int :: Int :: Int :: HNil[Id])#Mapped[Option] = Option(2) :: Option(3) :: Option(4) :: HNil[Option]
    if (l4.foldLeft(isDefined)(true)(_ & _)) {
      val l4a = l4 map get
      val p2 = hlprd(l4a)
      println(p2)
    }
    
    val sumO = hlistFnToFn2(liftO(hlsum))
    val prdO = hlistFnToFn3(liftO(hlprd))

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
  }
}

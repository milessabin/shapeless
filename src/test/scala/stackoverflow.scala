import org.junit.Test
import org.junit.Assert._

class StackOverflow1 {
  // http://stackoverflow.com/questions/7606587

  import HList._

  trait FoldCurry[L <: HList, F, Out] {
    def apply(l : L, f : F) : Out
  }
  
  implicit def foldCurry1[H, Out] = new FoldCurry[H :: HNil, H => Out, Out] {
    def apply(l : H :: HNil, f : H => Out) = f(l.head)
  }
  
  implicit def foldCurry2[H, T <: HList, FT, Out](implicit fct : FoldCurry[T, FT, Out]) = new FoldCurry[H :: T, H => FT, Out] {
    def apply(l : H :: T, f : H => FT) = fct(l.tail, f(l.head))
  }
  
  def foldCurry[L <: HList, F, Out](l : L, f : F)(implicit fc : FoldCurry[L, F, Out]) : Out = fc(l, f)
  
  def typed[T](t : => T) {}

  @Test
  def testFoldCurry {
    val f1 = (i : Int, j : Int, k : Int, l : Int) => i+j+k+l
    val f1c = f1.curried
    
    val l1 = 1 :: 2 :: 3 :: 4 :: HNil

    val r1 = foldCurry(l1, f1c)
    typed[Int](r1)
    assertEquals(10, r1)
    
    val f2 = (i : Int, s : String, d : Double) => (i+1, s.length, d*2)
    val f2c = f2.curried
    
    val l2 = 23 :: "foo" :: 2.0 :: HNil
    
    val r2 = foldCurry(l2, f2c)
    typed[(Int, Int, Double)](r2)
    assertEquals((24, 3, 4.0), r2)
  }
}

class StackOverflow2 {
  // http://stackoverflow.com/questions/8270526
  
  import HList._
  import Functions._
  import Traversables._
  
  sealed abstract class A { def eval() : A }
  case class A0 () extends A { def eval() = this }
  case class A1 ( a : A ) extends A  { def eval() = this }
  case class A2 ( a : A, b : A ) extends A  { def eval() = this }
  
  case class ApplyA[C, L <: HList](c : C, l : L)(implicit hl : FnHListerAux[C, L => A]) extends A {
    def eval () : A = hl(c)(l)
  }

  val c0 : () => A = A0
  val c1 : A => A = A1
  val c2 : (A, A) => A = A2
  
  val a : A = A0()
  
  val a0 = ApplyA(c0, HNil : HNil)
  val a1 = ApplyA(c1, a :: HNil)
  val a2 = ApplyA(c2, a :: a :: HNil)
}

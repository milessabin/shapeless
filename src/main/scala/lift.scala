object LiftOFn {
  import PolyFun._
  import HList._
  import Tuples._
  import Functions._

  def liftO[In <: HList, Out <: HList, R](f :  In => R)
    (implicit m : MapperAux[get.type, Out, In], lf : LeftFolder[Out, Boolean, isDefined.type]) : Out => Option[R] = 
      (o : Out) => if(o.foldLeft(true)(isDefined)(_ && _)) Some(f(o map get)) else None 
}

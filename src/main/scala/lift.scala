object LiftOFn {
  import PolyFun._
  import HList._
  import Functions._

  def liftO[InF, InL <: HList, R, OInL <: HList, OutF](f :  InF)
    (implicit
      hlister   : FnHListerAux[InF, InL => R],
      mapper    : MapperAux[get.type, OInL, InL],
      folder    : LeftFolder[OInL, Boolean, isDefined.type],
      unhlister : FnUnHListerAux[OInL => Option[R], OutF]
    ) : OutF = {
    val hf = f.hlisted
    val lifted =
      (o : OInL) =>
        if(o.foldLeft(true)(isDefined)(_ && _)) Some(hf(o map get))
        else None
    unhlister(lifted)
  }
}

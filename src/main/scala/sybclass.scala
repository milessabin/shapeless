object SybClass {
  import PolyFun._
  
  trait Data[HF <: HRFn, T, R] {
    def gmapQ(t : T) : List[R]
  }

  implicit def dfltData[HF <: HRFn, T, R] = new Data[HF, T, R] {
    def gmapQ(t : T) : List[R] = Nil
  }

  implicit def pairData[HF <: HRFn, R, T, U](implicit qt : Case[HF, T => R], qu : Case[HF, U => R]) = new Data[HF, (T, U), R] {
    def gmapQ(t : (T, U)) = List(qt.f(t._1), qu.f(t._2))
  }

  implicit def eitherData[HF <: HRFn, R, T, U](implicit qt : Case[HF, T => R], qu : Case[HF, U => R]) = new Data[HF, Either[T, U], R] {
    def gmapQ(t : Either[T, U]) = t match {
      case Left(t) => List(qt.f(t))
      case Right(u) => List(qu.f(u))
    }
  }

  implicit def optionData[HF <: HRFn, R, T](implicit qt : Case[HF, T => R]) = new Data[HF, Option[T], R] {
    def gmapQ(t : Option[T]) = t.map(qt.f).toList
  }

  implicit def listData[HF <: HRFn, R, T](implicit qt : Case[HF, T => R]) = new Data[HF, List[T], R] {
    def gmapQ(t : List[T]) = t.map(qt.f)
  }

  trait DataT[HF <: HRFn, T] {
    def gmapT(t : T) : T
  }

  implicit def dfltDataT[HF <: HRFn, T] : DataT[HF, T] = new DataT[HF, T] {
    def gmapT(t : T) = t
  }

  implicit def pairDataT[HF <: HRFn, T, U](implicit ft : Case[HF, T => T], fu : Case[HF, U => U]) = new DataT[HF, (T, U)] {
    def gmapT(t : (T, U)) = (ft.f(t._1), fu.f(t._2))
  }

  implicit def eitherDataT[HF <: HRFn, T, U](implicit ft : Case[HF, T => T], fu : Case[HF, U => U]) = new DataT[HF, Either[T, U]] {
    def gmapT(t : Either[T, U]) = t match {
      case Left(t) => Left(ft.f(t))
      case Right(u) => Right(fu.f(u))
    }
  }

  implicit def optionDataT[HF <: HRFn, T](implicit ft : Case[HF, T => T]) = new DataT[HF, Option[T]] {
    def gmapT(t : Option[T]) = t.map(ft.f)
  }

  implicit def listDataT[HF <: HRFn, T](implicit ft : Case[HF, T => T]) = new DataT[HF, List[T]] {
    def gmapT(t : List[T]) = t.map(ft.f)
  }

  case class Everything[HF <: HRFn, T, R](data : Data[HF, T, R], fT : Case[HF, T => R]) {
    def apply(k : (R, R) => R)(t : T) : R = (fT.f(t) /: data.gmapQ(t))(k) 
  }
  
  implicit def everythingDflt[HF <: HRFn, R, T](implicit data : Data[HF, T, R], fT : Case[HF, T => R]) = Everything(data, fT)
  
  def everything[HF <: HRFn](f : HF) = new {
    def apply[T, R](k : (R, R) => R)(t : T)(implicit eT : Everything[HF, T, R]) = eT(k)(t)
  }

  type Everywhere[HF <: HRFn, T] = Case[Everywhere0[HF], T => T]
  
  trait Everywhere0[HF <: HRFn] extends (Id ~> Id) with NoDefault
  
  implicit def everywhere0Dflt[HF <: HRFn, T](implicit data : DataT[Everywhere0[HF], T], fT : Case[HF, T => T]) =
    Case[Everywhere0[HF], T => T](t => fT.f(data.gmapT(t)))

  def everywhere[HF <: HRFn](f : HF) = new {
    def apply[T](t : T)(implicit eT : Everywhere[HF, T]) = eT.f(t)
  }
}

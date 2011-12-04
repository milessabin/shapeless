object SybClass {
  import PolyFun._
  
  trait Data[HF, T, R] {
    def gmapQ(t : T) : List[R]
  }
  
  def gmapQ[T, HF, R](f : HF)(t : T)(implicit data : Data[HF, T, R]) = data.gmapQ(t)

  implicit def dfltData[HF, T, R] = new Data[HF, T, R] {
    def gmapQ(t : T) : List[R] = Nil
  }

  implicit def pairData[HF, R, T, U](implicit qt : Case[HF, T => R], qu : Case[HF, U => R]) = new Data[HF, (T, U), R] {
    def gmapQ(t : (T, U)) = List(qt(t._1), qu(t._2))
  }

  implicit def eitherData[HF, R, T, U](implicit qt : Case[HF, T => R], qu : Case[HF, U => R]) = new Data[HF, Either[T, U], R] {
    def gmapQ(t : Either[T, U]) = t match {
      case Left(t) => List(qt(t))
      case Right(u) => List(qu(u))
    }
  }

  implicit def optionData[HF, R, T](implicit qt : Case[HF, T => R]) = new Data[HF, Option[T], R] {
    def gmapQ(t : Option[T]) = t.map(qt.value).toList
  }

  implicit def listData[HF, R, T](implicit qt : Case[HF, T => R]) = new Data[HF, List[T], R] {
    def gmapQ(t : List[T]) = t.map(qt.value)
  }
  
  trait DataT[HF, T] {
    def gmapT(t : T) : T
  }

  def gmapT[T, HF](f : HF)(t : T)(implicit data : DataT[HF, T]) = data.gmapT(t)

  implicit def dfltDataT[HF, T] : DataT[HF, T] = new DataT[HF, T] {
    def gmapT(t : T) = t
  }

  implicit def pairDataT[HF, T, U](implicit ft : Case[HF, T => T], fu : Case[HF, U => U]) = new DataT[HF, (T, U)] {
    def gmapT(t : (T, U)) = (ft(t._1), fu(t._2))
  }

  implicit def eitherDataT[HF, T, U](implicit ft : Case[HF, T => T], fu : Case[HF, U => U]) = new DataT[HF, Either[T, U]] {
    def gmapT(t : Either[T, U]) = t match {
      case Left(t) => Left(ft(t))
      case Right(u) => Right(fu(u))
    }
  }

  implicit def optionDataT[HF, T](implicit ft : Case[HF, T => T]) = new DataT[HF, Option[T]] {
    def gmapT(t : Option[T]) = t.map(ft.value)
  }

  implicit def listDataT[HF, T](implicit ft : Case[HF, T => T]) = new DataT[HF, List[T]] {
    def gmapT(t : List[T]) = t.map(ft.value)
  }

  type Everywhere[HF, T] = Case[Everywhere0[HF], T => T]
    
  trait Everywhere0[HF] extends (Id ~> Id) with NoDefault
    
  def everywhere[HF <: HRFn](f : HF) = new Everywhere0[f.type] {}
  
  implicit def everywhereDflt[HF, T](implicit data : DataT[Everywhere0[HF], T], fT : Case[HF, T => T]) =
    new Case[Everywhere0[HF], T => T](t => fT(data.gmapT(t)))
}

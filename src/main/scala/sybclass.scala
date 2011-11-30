object Data  {
  import PolyFun._
  
  trait Data[HF <: HRFn, R, T] {
    def gmapQ(t : T) : List[R]
  }

  implicit def dfltData[HF <: HRFn, R, T] = new Data[HF, R, T] {
    def gmapQ(t : T) : List[R] = Nil
  }

  implicit def pairData[HF <: HRFn, R, T, U](implicit qt : Case[HF, T => R], qu : Case[HF, U => R]) = new Data[HF, R, (T, U)] {
    def gmapQ(t : (T, U)) = List(qt.f(t._1), qu.f(t._2))
  }

  implicit def eitherData[HF <: HRFn, R, T, U](implicit qt : Case[HF, T => R], qu : Case[HF, U => R]) = new Data[HF, R, Either[T, U]] {
    def gmapQ(t : Either[T, U]) = t match {
      case Left(t) => List(qt.f(t))
      case Right(u) => List(qu.f(u))
    }
  }

  implicit def optionData[HF <: HRFn, R, T](implicit qt : Case[HF, T => R]) = new Data[HF, R, Option[T]] {
    def gmapQ(t : Option[T]) = t.map(qt.f).toList
  }

  implicit def listData[HF <: HRFn, R, T](implicit qt : Case[HF, T => R]) = new Data[HF, R, List[T]] {
    def gmapQ(t : List[T]) = t.map(qt.f)
  }
}

object Everything {
  import Data._
  import PolyFun._

  case class Everything[HF <: HRFn, R, T](data : Data[HF, R, T], fT : Case[HF, T => R]) {
    def apply(k : (R, R) => R)(t : T) : R = (fT.f(t) /: data.gmapQ(t))(k) 
  }
  
  implicit def everythingDflt[HF <: HRFn, R, T](implicit data : Data[HF, R, T], fT : Case[HF, T => R]) = Everything(data, fT)
  
  def everything[HF <: HRFn](f : HF) = new {
    def apply[T, R](k : (R, R) => R)(t : T)(implicit eT : Everything[HF, R, T]) = eT(k)(t)
  }
}

object Trans {
  import PolyFun._

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
}

object Everywhere {
  import Trans._
  import PolyFun._
  
  type Everywhere[HF <: HRFn, T] = Case[Everywhere0[HF], T => T]
  
  trait Everywhere0[HF <: HRFn] extends (Id ~> Id) with NoDefault
  
  implicit def everywhere0Dflt[HF <: HRFn, T](implicit data : DataT[Everywhere0[HF], T], fT : Case[HF, T => T]) =
    Case[Everywhere0[HF], T => T](t => fT.f(data.gmapT(t)))

  def everywhere[HF <: HRFn](f : HF) = new {
    def apply[T](t : T)(implicit eT : Everywhere[HF, T]) = eT.f(t)
  }
}

object GSizeAll {
  import Data._
  import PolyFun._

  object gsizeAll extends (Id ~> Const[Int]#λ) with NoDefault
  implicit def gsizeAllString = gsizeAll.λ[String](s => s.length)
  implicit def gsizeAllDflt[T](implicit data : Data[gsizeAll.type, Int, T]) = gsizeAll.λ[T](1+data.gmapQ(_).sum) 
}

object GSizeAll2 {
  import Everything._
  import PolyFun._

  object gsize extends (Id ~> Const[Int]#λ) {
    def default[T](t : T) = 1
  }
  implicit def gsizeString = gsize.λ[String](s => s.length)
  
  def gsizeAll2[T](t : T)(implicit e : Everything[gsize.type, Int, T]) : Int = everything(gsize)((_ : Int)+(_ : Int))(t) 
}

object IncAll {
  import Trans._
  import PolyFun._

  object incAll extends (Id ~> Id) with NoDefault
  implicit def incAllInt = incAll.λ[Int](_+1)
  implicit def incAllString = incAll.λ[String](_+"*")
  implicit def incAllDflt[T](implicit data : DataT[incAll.type, T]) = incAll.λ[T](data.gmapT)
}

object IncAll2 {
  import PolyFun._
  import Everywhere._

  object inc extends (Id ~> Id) {
    def default[T](t : T) = t
  }
  implicit def incInt = inc.λ[Int](_+1)
  implicit def incString = inc.λ[String](_+"*")

  def incAll2[T](t : T)(implicit e : Everywhere[inc.type, T]) : T = everywhere(inc)(t)
}

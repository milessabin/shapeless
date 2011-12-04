object PolyFun {
  type Id[T] = T
  type Const[C] = {
    type λ[T] = C
  }

  case class Case[-P, T](t : T) {
    def value = t
    def apply[F, G](f : F)(implicit ev : T <:< (F => G)) : G = t(f)
  }
  
  trait Poly {
    type TC[_]

    type λ[T] = Case[this.type, TC[T]]
    def λ[T](c : TC[T]) = Case[this.type, TC[T]](c)
  }
  
  trait PolyVal[TC0[_]] extends Poly {
    type TC[X] = TC0[X]
    def apply[T](implicit c : λ[T]) : TC[T] = c.value
  }

  trait HRFn extends Poly {
    type TC[T] = F[T] => G[T]
    type F[_]
    type G[_]

    def default[T](f : F[T]) : G[T]
    implicit def defaultCase[T] = λ[T](default)
    
    def apply[T](f : F[T])(implicit c : λ[T] = defaultCase[T]) : G[T] = c(f)
  }
  
  trait ~>[F0[_], G0[_]] extends HRFn {
    type F[X] = F0[X]
    type G[X] = G0[X]
  }
  
  trait NoDefault extends HRFn {
    def default[T](f : F[T]) : G[T] = {
      sys.error("No default case for: "+getClass.getName+"@"+f.getClass.getName)
    }
  }

  implicit def univInstFn[HF <: HRFn, T](h : HF)(implicit c : h.λ[T] = h.defaultCase[T]) : h.TC[T] = c.value

  object identity extends (Id ~> Id) {
    def default[T](t : T) = t
  }

  object singleton extends (Id ~> Set) {
    def default[T](t : T) = Set(t)
  }

  object choose extends (Set ~> Option) {
    def default[T](s : Set[T]) = s.headOption 
  }

  object list extends (Id ~> List) {
    def default[T](t : T) = List(t)
  }
  
  object headOption extends (List ~> Option) {
    def default[T](l : List[T]) = l.headOption
  }
  
  object isDefined extends (Option ~> Const[Boolean]#λ) {
    def default[T](o : Option[T]) = o.isDefined
  }
  
  object get extends (Option ~> Id) {
    def default[T](o : Option[T]) = o.get
  }
  
  object option extends (Id ~> Option) {
    def default[T](t : T) = Option(t)
  }
  
  object zero extends PolyVal[Id]
  implicit def intZero = zero.λ[Int](0) 
  implicit def stringZero = zero.λ[String]("") 
  implicit def listZero[T] = zero.λ[List[T]](Nil) 
}

object PolyFun {
  type Id[T] = T
  type Const[C] = {
    type λ[T] = C
  }

  case class Case[HF <: HRFn, F](f : F)
  
  trait HRFn {
    type F[_]
    type G[_]

    type Fn[T] = F[T] => G[T]
    type λ[T] = Case[this.type , Fn[T]]
    def λ[T](c : F[T] => G[T]) = Case[this.type, Fn[T]](c)
    
    def default[T](f : F[T]) : G[T]
    implicit def defaultCase[T] = λ[T](default)
    
    def apply[T](f : F[T])(implicit c : λ[T] = defaultCase[T]) : G[T] = c.f(f)
  }
  
  trait ~>[F0[_], G0[_]] extends HRFn {
    type F[X] = F0[X]
    type G[X] = G0[X]
  }

  implicit def univInst[HF <: HRFn, T](h : HF)(implicit c : h.λ[T] = h.defaultCase[T]) : h.Fn[T] = c.f

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
  
  object toInt extends (Id ~> Const[Int]#λ) {
    def default[T](t : T) = t.toString.toInt
  }
  
  object size extends (Id ~> Const[Int]#λ) {
    def default[T](t : T) = 1
  }
  implicit def sizeInt = size.λ[Int](x => 1)
  implicit def sizeString = size.λ[String](s => s.length)
  implicit def sizeList[T] = size.λ[List[T]](l => l.length)
  implicit def sizeOption[T](implicit cases : size.λ[T]) = size.λ[Option[T]](t => 1+size(t.get))
  implicit def sizeTuple[T, U](implicit st : size.λ[T], su : size.λ[U]) = size.λ[(T, U)](t => size(t._1)+size(t._2))
  
  def main(args : Array[String]) {
    implicitly[choose.λ[Int]]
    implicitly[Case[choose.type, Set[Int] => Option[Int]]]
    
    implicitly[size.λ[Int]]
    implicitly[Case[size.type, Option[Int] => Int]]

    implicitly[option.λ[Int]]
    implicitly[Case[option.type, Int => Option[Int]]]
    implicitly[Case[option.type, Id[Int] => Option[Int]]]

    implicitly[singleton.λ[Int]]
    implicitly[Case[singleton.type, Int => Set[Int]]]
    implicitly[Case[singleton.type, Id[Int] => Set[Int]]]

    val si = size(23)
    println(si)
    
    val ss = size("foo")
    println(ss)
    
    val sl = size(List(1, 2, 3))
    println(sl)
    
    val so = size(Option(23))
    println(so)

    val st = size((23, "foo"))
    println(st)
    
    val ls = List("foo", "bar", "baz")
    val lss = ls map size
    val lsi = ls map identity
    
    val is = identity("foo")

    // Direct application
    val s1 = singleton(23)
    println(s1)
    val s2 = singleton("foo")
    println(s2)
    
    def app[G[_]](f : Int => G[Int]) = f(23)
    val as = app(singleton)
    val al = app(list)
    
    // Implicit conversion to monomorphic function values
    val l1 = List(1, 2, 3) map singleton
    println(l1)
    val l2 = List("foo", "bar", "baz") map list
    println(l2)
    val l3 = List(List(1), List(2), List(4)) map headOption
    println(l3)

    // Use as polymorphic function values
    def pairApply[G[_]](f : Id ~> G) = (f(23), f("foo"))

    val a1 = pairApply(singleton)
    println(a1)
    val a2 = pairApply(list)
    println(a2)
    val a3 = pairApply[Const[Int]#λ](size)
    println(a3)
    
    // Use as polymorphic function values with type specific cases
    def pairApply2[G[_]](f : Id ~> G)(implicit  fi : f.λ[Int], fs : f.λ[String]) = (f(23), f("foo"))
    
    val a4 = pairApply2(singleton)
    println(a4)
    val a5 = pairApply2(list)
    println(a5)
    val a6 = pairApply2[Const[Int]#λ](size)
    println(a6)

    def pairMap[F[_]](f : Id ~> F) = (List(1, 2, 3) map f, List("foo", "bar", "baz") map f)
  
    val m1 = pairMap(singleton)
    println(m1)
    val m2 = pairMap(list)
    println(m2)
    
    val l5 = List(1, 2, 3)
    val l6 = l5 map option
    println(l6)
    
    val l7 = l6 map isDefined
    println(l7)
    
    val lsi2 = List(Set(1), Set(2), Set(3))
    val loi2 = lsi2 map choose
  }
}

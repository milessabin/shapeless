object Rank2Poly {
  type Id[T] = T
  type Const[C] = {
    type λ[T] = C
  }

  trait ~>[F[_], G[_]] {
    type λ[T] = F[T] => G[T]
    def apply[T](x : F[T]) : G[T]
    def univInst[T] : λ[T] = apply _ 
  }

  // Use of dependent type h.λ[T] essential here
  implicit def univInst[F[_], G[_], T](h : F ~> G) : h.λ[T] = h.univInst

  object singleton extends (Id ~> Set) {
    def apply[T](t : T) = Set(t)
  }

  object choose extends (Set ~> Option) {
    def apply[T](s : Set[T]) = s.headOption
  }

  object list extends (Id ~> List) {
    def apply[T](t : T) = List(t)
  }
  
  object headOption extends (List ~> Option) {
    def apply[T](t : List[T]) = t.headOption
  }
  
  object isDefined extends (Option ~> Const[Boolean]#λ) {
    def apply[T](o : Option[T]) = o.isDefined
  }
  
  object get extends (Option ~> Id) {
    def apply[T](o : Option[T]) : T = o.get
  }
  
  object option extends (Id ~> Option) {
    def apply[T](t : T) : Option[T] = Option(t)
  }
  
  def main(args : Array[String]) {
    
    // Direct application
    val s1 = singleton(23)
    println(s1)
    val s2 = singleton("foo")
    println(s2)
    
    // Implicit conversion to monomorphic function values
    val l1 = List(1, 2, 3) map singleton
    println(l1)
    val l2 = List("foo", "bar", "baz") map list
    println(l2)
    val l3 = List(List(1), List(2), List(4)) map headOption
    println(l3)

    // Use as polymorphic function values
    def pairApply[F[_]](f : Id ~> F) = (f(23), f("foo"))

    val a1 = pairApply(singleton)
    println(a1)
    val a2 = pairApply(list)
    println(a2)

    def pairMap[F[_]](f : Id ~> F) = (List(1, 2, 3) map f, List("foo", "bar", "baz") map f)
  
    val m1 = pairMap(singleton)
    println(m1)
    val m2 = pairMap(list)
    println(m2)
  }
}

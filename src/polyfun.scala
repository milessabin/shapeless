object PolyFun {
  type Id[T] = T
  type Const[C] = {
    type λ[T] = C
  }

  trait HRFn
  
  trait ~>[F[_], G[_]] extends HRFn {
    type λ[T] = F[T] => G[T]
    def apply[T](x : F[T]) : G[T]
  }

  // Use of dependent type h.λ[T] essential here
  implicit def univInst1[F[_], G[_], T](h : F ~> G) : h.λ[T] = h.apply _
  implicit def univInst2[F[_], Out, T](h : F ~> Const[Out]#λ) : h.λ[T] = h.apply _

  trait HFn2[F1[_], F2[_], G[_]] {
    type λ[T] = (F1[T], F2[T]) => G[T]
    def apply[T](x : F1[T], y : F2[T]) : G[T]
  }
  
  // Use of dependent type h.λ[T] essential here
  implicit def univInst21[F1[_], F2[_], G[_], T](h : HFn2[F1, F2, G]) : h.λ[T] = h.apply _
  implicit def univInst22[F1[_], F2[_], Out, T](h : HFn2[F1, F2, Const[Out]#λ]) : h.λ[T] = h.apply _

  object identity extends (Id ~> Id) {
    def apply[T](t : T) = t
  }

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
    def apply[T](o : Option[T]) : Boolean = o.isDefined
  }
  
  object get extends (Option ~> Id) {
    def apply[T](o : Option[T]) : T = o.get
  }
  
  object option extends (Id ~> Option) {
    def apply[T](t : T) : Option[T] = Option(t)
  }
  
  object toInt extends (Id ~> Const[Int]#λ) {
    def apply[T](t : T) : Int = t.toString.toInt
  }
  
  object sum extends HFn2[Id, Id, Const[Int]#λ] {
    def apply[T](x : T, y : T) : Int = toInt(x)+toInt(y)
  }
  
  def main(args : Array[String]) {
    
    // Direct application
    val s1 = singleton(23)
    println(s1)
    val s2 = singleton("foo")
    println(s2)
    
    def app[G[_]](f : Int => G[Int]) = f(23)
    app(singleton)
    
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
    
    val l4 = List(1, 2, 3)
    val sm1 = l4.foldLeft(0)(sum)
    println(sm1)
    
    println(sum(23, 45))
    println(sum("2", "5"))
    
    def combine(f : (Int, Int) => Int) = f(2, 3)
    println(combine(sum))
    
    val l5 = List(1, 2, 3)
    val l6 = l5 map option
    println(l6)
    
    val l7 = l6 map isDefined
    println(l7)
  }
}

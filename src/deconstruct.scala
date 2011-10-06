object Deconstruct {
  
  import Rank2Poly._
  
  type H = Option[Int]
  
  trait Invert[FT, T0] { type T = T0 }
  implicit def inv[F[_], T] = new Invert[F[T], T] {}
  
  implicitly[Invert[Option[Int], Int]]
  
  def inverter[FT, T](ft : FT)(implicit ev : Invert[FT, T]) = ev 
  
  def foo[FT, T](ft : FT)(implicit ev : Invert[FT, T]) : T = sys.error("TODO")

  val o = Option(23)
  val i = foo(o)
  
  val iv = inverter(o)
  
  implicitly[iv.T =:= Int]
  
  val i2 = get(o)
  
  def app[F[_], G[_], I, X](f : F ~> G)(x : X)(implicit ev : X <:< F[I]) = f(x)
  
  val i3 : Int = app(get)(o)
  
  val o2 : Option[Int] = app(option)(23)
  
  val o3 : Option[Int] = app(headOption)(List(23))

  val o4 : Option[Int] = app(choose)(Set(23))
}

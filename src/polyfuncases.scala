trait LowPriorityPolyFunCases {
  import HList._
  
  type Id[T] = T
  type Const[C] = {
    type λ[T] = C
  }

  type Tagged[T] = { type Tag = T }
  type @@[T, U] = T with Tagged[U]
  
  def tag[T] = new {
    def apply[U](u : U) : U with Tagged[T] = u.asInstanceOf[U @@ T]
  }
  
  trait HRFn
  
  trait ~>[F[_], G[_]] {
    type λ[T] = (F[T] => G[T]) @@ this.type
    def λ[T](c : F[T] => G[T]) = tag[this.type](c)
    
    def apply[F, G](f : F)(implicit c : (F => G) @@ this.type) : G = c(f)
  }
}

object PolyFunCases extends LowPriorityPolyFunCases {
}

object TestPolyFunCases {
  import HList._
  import PolyFunCases._
  
  object size extends (Id ~> Const[Int]#λ)

  implicit def sizeInt = size.λ[Int](x => 1)
  implicit def sizeString = size.λ[String](s => s.length)
  implicit def sizeList[T] = size.λ[List[T]](l => l.length)
  implicit def sizeOption[T](implicit cases : size.λ[T]) = size.λ[Option[T]](t => 1+size(t.get))
  implicit def sizeTuple[T, U](implicit st : size.λ[T], su : size.λ[U]) = size.λ[(T, U)](t => size(t._1)+size(t._2))
  
  def main(args : Array[String]) {
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
  }
}

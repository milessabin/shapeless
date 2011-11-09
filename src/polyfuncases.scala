object PolyFunCases {
  type Id[T] = T
  type Const[C] = {
    type λ[T] = C
  }

  trait HRFn

  type Tagged[T] = { type Tag = T }
  type @@[T, U] = T with Tagged[U]
  
  def tag[T] = new {
    def apply[U](u : U) : U with Tagged[T] = u.asInstanceOf[U @@ T]
  }
  
  trait ~~>[F[_], G[_]] extends HRFn {
    type Case[T] <: F[T] => G[T]
    def apply[T](x : F[T])(implicit f : Case[T]) : G[T] = f(x) 
  }

  object size extends (Id ~~> Const[Int]#λ) {
    type Case[T] = (T => Int) @@ size.type 
  }
  
  implicit def sizeInt : size.Case[Int] = tag(x => 1)
  implicit def sizeString : size.Case[String] = tag(s => s.length)
  implicit def sizeList[T] : size.Case[List[T]] = tag(l => l.length)
  implicit def sizeTuple[T, U](implicit sizeT : size.Case[T], sizeU : size.Case[U]) : size.Case[(T, U)] = tag(t => size(t._1)+size(t._2)) 

  def main(args : Array[String]) {
    val si = size(23)
    println(si)
    
    val ss = size("foo")
    println(ss)
    
    val sl = size(List(1, 2, 3))
    println(sl)
    
    val st = size((23, "foo"))
    println(st)
  }
}

package shapeless.examples

object SizedExample1 extends App {
  import shapeless.Nat
  import shapeless.Nat._
  import shapeless.Sized
  import shapeless.Sized._
  
  def sequence[T](lo : List[Option[T]]) = if (lo.exists(_ isEmpty)) None else Some(lo.map(_.get))
  
  val hdrs = List("Title", "Author")
  val rows = List(List("foo", "Mr X"), List("bar", "Mr Y"))
  
  def row(cols : List[String]) = cols.mkString("\"", "\", \"", "\"")
  
  def csv[N <: Nat](hdrs : Sized[List[String], N], rows : List[Sized[List[String], N]]) =
    row(hdrs) :: (rows map (r => row(r)))
  
  val sizedHdrs = hdrs.sized[_2]
  val sizedRows = sequence(rows map (_.sized[_2]))
    
  val formatted = 
    for {
      h  <- sizedHdrs 
      rs <- sizedRows
    } yield csv(h, rs)
    
  formatted map (_ foreach println)
}

object SizedExample2 {
  import shapeless.Nat
  import shapeless.Nat._
  import shapeless.Sized
  import shapeless.Sized._

  val hdrs = Sized[Seq]("Title", "Author")  // Sized[List[Int], _2]
  val rows = List(                          // List[Sized[List[Int], _2]]
    Sized[Seq]("foo", "Mr X"),
    Sized[Seq]("bar", "Mr Y")
  )

  def row(cols : Seq[String]) = cols.mkString("\"", "\", \"", "\"")
  
  def csv[N <: Nat](hdrs : Sized[Seq[String], N], rows : List[Sized[Seq[String], N]]) =
    row(hdrs) :: (rows map (r => row(r)))
    
  val formatted = csv(hdrs, rows)
  formatted foreach println
  
  val threeHdrs = Sized[Seq]("Title", "Author", "ISBN") // Sized[List[Int], _3]
  //val badFormatted = csv(threeHdrs, rows) // Does not compile
  
  val f : (Int*) => List[Int] = List.apply
}

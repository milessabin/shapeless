package shapeless.examples

object SizedExample1 extends App {
  import shapeless.Nat
  import shapeless.Nat._
  import shapeless.Sized
  import shapeless.Sized._
  
  def sequence[T](lo : List[Option[T]]) = if (lo.exists(_ isEmpty)) None else Some(lo.map(_.get))
  
  def row(cols : Seq[String]) = cols.mkString("\"", "\", \"", "\"")
  
  def csv[N <: Nat](hdrs : Sized[Seq[String], N], rows : List[Sized[Seq[String], N]]) =
    row(hdrs) :: rows.map(row(_))
  
  val hdrs = List("Title", "Author")
  val rows = List(List("foo", "Mr X"), List("bar", "Mr Y"))
  
  val threeHdrs = List("Title", "Author", "ISBN")

  for {
    shdrs <- hdrs.sized[_2] 
    srows <- sequence(rows map (_.sized[_2]))
  } {
    val formatted = csv(shdrs, srows)
    formatted foreach println
  }

  for {
    shdrs <- threeHdrs.sized[_2] 
    srows <- sequence(rows map (_.sized[_2]))
  } {
    // Not reached
    val formatted = csv(shdrs, srows)
    formatted foreach println
  }
}

object SizedExample2 extends App {
  import shapeless.Nat
  import shapeless.Nat._
  import shapeless.Sized
  import shapeless.Sized._

  def row(cols : Seq[String]) = cols.mkString("\"", "\", \"", "\"")
  
  def csv[N <: Nat](hdrs : Sized[Seq[String], N], rows : List[Sized[Seq[String], N]]) =
    row(hdrs) :: rows.map(row(_))
    
  val hdrs = Sized("Title", "Author")  // Sized[List[String], _2]
  val rows = List(                     // List[Sized[List[String], _2]]
    Sized("foo", "Mr X"),
    Sized("bar", "Mr Y")
  )

  val formatted = csv(hdrs, rows)
  formatted foreach println
  
  val threeHdrs = Sized[Seq]("Title", "Author", "ISBN") // Sized[List[Int], _3]
  //val badFormatted = csv(threeHdrs, rows) // Does not compile
}

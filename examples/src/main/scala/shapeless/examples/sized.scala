package shapeless.examples

object SizedExamples extends App {
  import shapeless.Nat
  import shapeless.Nat._
  import shapeless.Sized
  import shapeless.Sized._

  def sequence[T](lo : List[Option[T]]) = if (lo.exists(_ isEmpty)) None else Some(lo.map(_.get))
  
  def row(cols : Seq[String]) = cols.mkString("\"", "\", \"", "\"")
  
  def csv[N <: Nat](hdrs : Sized[Seq[String], N], rows : List[Sized[Seq[String], N]]) =
    row(hdrs) :: rows.map(row(_))
  
  def fullyStatic {
    val hdrs = Sized("Title", "Author")  // Sized[IndexedSeq[String], _2]
    val rows = List(                     // List[Sized[IndexedSeq[String], _2]]
      Sized("Types and Programming Languages", "Benjamin Pierce"),
      Sized("The Implementation of Functional Programming Languages", "Simon Peyton-Jones")
    )
  
    // hdrs and rows statically known to have the name number of columns
    val formatted = csv(hdrs, rows)
    formatted foreach println
    
    // threeHdrs has the wrong number of columns for rows
    val threeHdrs = Sized("Title", "Author", "ISBN")  // Sized[IndexedSeq[Int], _3]
    //val badFormatted = csv(threeHdrs, rows)         // Does not compile
  }

  def mixedDynamicStatic {
    val hdrs = List("Title", "Author")
    val rows = List(
      List("Types and Programming Languages", "Benjamin Pierce"),
      List("The Implementation of Functional Programming Languages", "Simon Peyton-Jones")
    )
    
    val threeHdrs = List("Title", "Author", "ISBN")
  
    for {
      shdrs <- hdrs.sized[_2] 
      srows <- sequence(rows map (_.sized[_2]))
    } {
      // If we get here then our lists are statically know to be
      // of the appropriate sizes
      val formatted = csv(shdrs, srows)
      formatted foreach println
    }
  
    for {
      shdrs <- threeHdrs.sized[_2]   // This will be empty ... 
      srows <- sequence(rows map (_.sized[_2]))
    } {
      // ... hence, not reached
      val formatted = csv(shdrs, srows)
      formatted foreach println
    }
  }
  
  println("Fully static: ")
  fullyStatic
  
  println
  
  println("Mixed dynamic/static")
  mixedDynamicStatic
}

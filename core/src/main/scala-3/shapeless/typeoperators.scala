package shapeless

trait theScalaCompat {
  def apply[T](implicit t: T): T = ???

  def selectDynamic(tpeSelector: String): Any = ???
}

trait TypeOfScalaCompat {
  def selectDynamic(code: String): Any = ???
}

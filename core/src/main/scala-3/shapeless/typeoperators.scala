package shapeless

trait theScalaCompat {
  transparent inline def the[T](using inline x: T): x.type = x
}

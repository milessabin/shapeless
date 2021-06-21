package shapeless

trait theScalaCompat {
  transparent inline def summon[T](using inline x: T): x.type = x
}

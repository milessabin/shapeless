package shapeless

trait DefaultScalaCompat {
  implicit def materialize[T, L <: HList]: Default.Aux[T, L] = ???
}

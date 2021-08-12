package shapeless

package GenericTestsAux
object GenericTestsAuxScalaCompat {
  type TapRepr[A] = ConstTap[A] :+: InTap[A, Nothing] :+: OutTap[A, Any] :+: PipeTap[A, Any] :+: CNil
}

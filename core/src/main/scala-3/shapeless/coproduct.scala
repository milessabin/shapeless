package shapeless

trait CoproductScalaCompat {

  type CoproductToTuple[C <: Coproduct] <: scala.Tuple = C match {
    case CNil    => EmptyTuple
    case h :+: t => h *: CoproductToTuple[t]
  }
  type TupleToCoproduct[T <: scala.Tuple] <: Coproduct = T match {
    case EmptyTuple => CNil
    case h *: t     => h :+: TupleToCoproduct[t]
  }

  type HListToCoproduct[L <: HList] <: Coproduct = L match {
    case HNil   => CNil
    case h :: t => h :+: t
  }

  def extractCoproduct[C <: Coproduct](coproduct: C): scala.Tuple.Union[CoproductToTuple[C]] = coproduct match {
    case Inl(head) => head.asInstanceOf
    case Inr(tail) => extractCoproduct(tail.asInstanceOf)
    case err: CNil => err.impossible
  }

  def coproductFromOrdinal[T <: scala.Tuple](a: scala.Tuple.Union[T], ordinal: Int): TupleToCoproduct[T] =
    if ordinal == 0 then Inl(a).asInstanceOf
    else Inr(coproductFromOrdinal(a, ordinal - 1)).asInstanceOf
}

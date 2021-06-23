package shapeless

trait HListScalaCompat {

  type TupleToHList[T <: scala.Tuple] <: HList = T match {
    case EmptyTuple => HNil
    case h *: t     => h :: TupleToHList[t]
  }

  //TODO: tailrec
  def tupleToHList[T <: scala.Tuple](tuple: T): TupleToHList[T] = tuple match {
    case _: EmptyTuple  => HNil
    case cons: *:[h, t] => ::(cons.head, tupleToHList(cons.tail))
  }

  type HListToTuple[L <: HList] <: scala.Tuple = L match {
    case HNil   => EmptyTuple
    case h :: t => h *: HListToTuple[t]
  }
  //TODO: tailrec
  def hListToTuple[L <: HList](hlist: L): HListToTuple[L] = hlist match {
    case _: HNil        => EmptyTuple
    case cons: ::[h, t] => (cons.head *: hListToTuple(cons.tail))
  }
}

package shapeless

trait HListScalaCompat {

  type TupleToHList[T <: scala.Tuple] <: HList = T match {
    case *:[h, t]     => h :: TupleToHList[t]
    case EmptyTuple.type => HNil
  }

  //TODO: tailrec
  def tupleToHList[T <: scala.Tuple](tuple: T): TupleToHList[T] = tuple match {
    case cons: *:[h, t] => ::(cons.head.asInstanceOf[h], tupleToHList(cons.tail.asInstanceOf[t]))
    case _: EmptyTuple.type  => HNil
  }

  type HListToTuple[L <: HList] <: scala.Tuple = L match {
    case h :: t => h *: HListToTuple[t]
    case HNil   => EmptyTuple
  }
  //TODO: tailrec
  def hListToTuple[L <: HList](hlist: L): HListToTuple[L] = hlist match {
    case cons: ::[h, t] => cons.head *: hListToTuple(cons.tail)
    case _: HNil        => EmptyTuple
  }
}

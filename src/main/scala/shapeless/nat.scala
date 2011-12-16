sealed trait Nat
sealed case class Succ[P <: Nat]() extends Nat

object Nat {
  final class _0 extends Nat
  type _1 = Succ[_0]
  type _2 = Succ[_1]
  type _3 = Succ[_2]
  type _4 = Succ[_3]
  type _5 = Succ[_4]
  type _6 = Succ[_5]
  type _7 = Succ[_6]
  type _8 = Succ[_7]
  type _9 = Succ[_8]
  type _10 = Succ[_9]
  type _11 = Succ[_10]
  type _12 = Succ[_11]
  type _13 = Succ[_12]
  type _14 = Succ[_13]
  type _15 = Succ[_14]
  type _16 = Succ[_15]
  type _17 = Succ[_16]
  type _18 = Succ[_17]
  type _19 = Succ[_18]
  type _20 = Succ[_19]
  type _21 = Succ[_20]
  type _22 = Succ[_21]

  val _0 = new _0
  val _1 = new _1
  val _2 = new _2
  val _3 = new _3
  val _4 = new _4
  val _5 = new _5
  val _6 = new _6
  val _7 = new _7
  val _8 = new _8
  val _9 = new _9
  val _10 = new _10
  val _11 = new _11
  val _12 = new _12
  val _13 = new _13
  val _14 = new _14
  val _15 = new _15
  val _16 = new _16
  val _17 = new _17
  val _18 = new _18
  val _19 = new _19
  val _20 = new _20
  val _21 = new _21
  val _22 = new _22
  
  trait Pred[A <: Nat, B <: Nat]
  implicit def pred[A <: Nat] = new Pred[Succ[A], A] {}
  
  trait Sum[A <: Nat, B <: Nat, C <: Nat]
  implicit def sum1[B <: Nat] = new Sum[_0, B, B] {}
  implicit def sum2[A <: Nat, B <: Nat, C <: Nat](implicit ev : Sum[A, Succ[B], C]) = new Sum[Succ[A], B, C] {}

  trait Prod[A <: Nat, B <: Nat, C <: Nat]
  implicit def prod1[B <: Nat] = new Prod[_0, B, _0] {}
  implicit def prod2[A <: Nat, B <: Nat, C <: Nat, D <: Nat](implicit ev1 : Prod[A, B, C], ev2 : Sum[B, C, D]) = new Prod[Succ[A], B, D] {}
}

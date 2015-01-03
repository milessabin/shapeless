package shapeless
package ops

import labelled._

object generic {  
  trait WithLabels[Repr, L <: HList] {
    type Out
    def to(r: Repr): Out
    def from(out: Out): Repr
  }

  object WithLabels {
    type Aux[Repr, L <: HList, Out0] = WithLabels[Repr, L] { type Out = Out0 }

    def apply[Repr, L <: HList](implicit withLabels: WithLabels[Repr, L]): Aux[Repr, L, withLabels.Out] = withLabels

    implicit val hnilWithLabels: Aux[HNil, HNil, HNil] =
      new WithLabels[HNil, HNil] {
        type Out = HNil
        def to(r: HNil) = HNil
        def from(out: HNil) = HNil
      }

    implicit def hconsWithLabels[H, T <: HList, LH <: Symbol, LT <: HList, TL <: HList](implicit
      tailWithLabels: WithLabels.Aux[T, LT, TL]
    ): Aux[H :: T, LH :: LT, FieldType[LH, H] :: TL] =
      new WithLabels[H :: T, LH :: LT] {
        type Out = FieldType[LH, H] :: TL
        def to(r: H :: T) = field[LH](r.head) :: tailWithLabels.to(r.tail)
        def from(out: FieldType[LH, H] :: TL) = out.head :: tailWithLabels.from(out.tail)
      }

    implicit val cnilWithLabels: Aux[CNil, HNil, CNil] =
      new WithLabels[CNil, HNil] {
        type Out = CNil
        def to(r: CNil) = r
        def from(out: CNil) = out
      }

    implicit def cconsWithLabels[H, T <: Coproduct, LH <: Symbol, LT <: HList, TL <: Coproduct](implicit
      tailWithLabels: WithLabels.Aux[T, LT, TL]
    ): Aux[H :+: T, LH :: LT, FieldType[LH, H] :+: TL] =
      new WithLabels[H :+: T, LH :: LT] {
        type Out = FieldType[LH, H] :+: TL
        def to(r: H :+: T) = r match {
          case Inl(h) => Inl(field[LH](h))
          case Inr(t) => Inr(tailWithLabels.to(t))
        } 
        def from(out: FieldType[LH, H] :+: TL) = out match {
          case Inl(h) => Inl(h)
          case Inr(t) => Inr(tailWithLabels.from(t))
        }  
      }
  }
}

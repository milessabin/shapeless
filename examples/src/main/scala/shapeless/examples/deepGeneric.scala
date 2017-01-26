package shapeless.examples

import shapeless._, test._

trait DeepGeneric[T] {
  type Repr

  def to(t: T): Repr
  def from(t: Repr): T
}

trait LowPriorityDeepGeneric {
  implicit def hlistHeadNotGeneric[H, T <: HList, TR <: HList](implicit dgt: Lazy[DeepGeneric.Aux[T, TR]]): DeepGeneric.Aux[H :: T, H :: TR] =
    new DeepGeneric[H :: T] {
      type Repr = H :: TR
      def to(hlist: H :: T): Repr = hlist match {
        case h :: t => h :: dgt.value.to(t)
      }
      def from(r: Repr): H :: T = r match {
        case h :: t => h :: dgt.value.from(t)
      }
    }
}

object DeepGeneric extends LowPriorityDeepGeneric {
  type Aux[T, Repr0] = DeepGeneric[T] { type Repr = Repr0 }

  implicit val hnilDeepGeneric: DeepGeneric.Aux[HNil, HNil] = new DeepGeneric[HNil] {
    type Repr = HNil
    def to(h: HNil): HNil = h
    def from(h: HNil): HNil = h
  }

  implicit def hlistDeepGeneric[H, HG, T <: HList, TR <: HList, HR](
    implicit hg: Generic.Aux[H, HG],
    hgen: Lazy[DeepGeneric.Aux[HG, HR]],
    tgen: Lazy[DeepGeneric.Aux[T, TR]]
  ): DeepGeneric.Aux[H :: T, HR :: TR] = new DeepGeneric[H :: T] {
    type Repr = HR :: TR
    def to(hlist: H :: T): Repr = hlist match {
      case h :: t => hgen.value.to(hg.to(h)) :: tgen.value.to(t)
    }
    def from(r: Repr): H :: T = r match {
      case h :: t => hg.from(hgen.value.from(h)) :: tgen.value.from(t)
    }
  }

  // implicit val cnilDeepGeneric: DeepGeneric.Aux[CNil, CNil] = new DeepGeneric[CNil] {
  //   type Repr = CNil
  //   def to(h: CNil): CNil = h
  //   def from(h: CNil): CNil = h
  // }
  // implicit def coproductDeepGeneric[H, HG, T <: Coproduct, TR <: Coproduct, HR <: Coproduct](
  //   implicit hg: Generic.Aux[H, HG],
  //   hgen: Lazy[DeepGeneric.Aux[HG, HR]],
  //   tgen: Lazy[DeepGeneric.Aux[T, TR]]
  // ): DeepGeneric.Aux[H :+: T, HR :+: TR] = new DeepGeneric[H :+: T] {
  //   type Repr = HR :+: TR
  //   def to(coproduct: H :+: T): Repr = coproduct match {
  //     case Inl(h) => Inl(hgen.value.to(hg.to(h)))
  //     case Inr(t) => Inr(tgen.value.to(t))
  //   }
  //   def from(r: Repr): H :+: T = r match {
  //     case Inl(h) => Inl(hg.from(hgen.value.from(h)))
  //     case Inr(t) => Inr(tgen.value.from(t))
  //   }
  // }

  // implicit def coproductDeepGeneric[H, HG, T <: Coproduct, TR <: Coproduct, HR](
  //   implicit hg: Generic.Aux[H, HG],
  //   hgen: Lazy[DeepGeneric.Aux[HG, HR]],
  //   tgen: Lazy[DeepGeneric.Aux[T, TR]]
  // ): DeepGeneric.Aux[H :+: T, HR :+: TR] = ???

  def apply[R](implicit dg: DeepGeneric[R]): Aux[R, dg.Repr] = dg
}

object DeepGenericDemo {
  case class A(x: Int, y: String)
  case class B(x: A, y: A)
  case class C(b: B, a: A)
  // case class D(a: E, b: B)

  type ARepr = Int :: String :: HNil
  type BRepr = ARepr :: ARepr :: HNil
  type CRepr = BRepr :: ARepr :: HNil
  // type ERepr = BRepr :+: CNil
  // type DRepr = ERepr :: BRepr :: HNil

  val adg = DeepGeneric[A :: HNil]
  typed[DeepGeneric.Aux[A :: HNil, ARepr :: HNil]](adg)

  val bdg = DeepGeneric[B :: HNil]
  typed[DeepGeneric.Aux[B :: HNil, BRepr :: HNil]](bdg)

  val cdg = DeepGeneric[C :: HNil]
  typed[DeepGeneric.Aux[C :: HNil, CRepr :: HNil]](cdg)
}

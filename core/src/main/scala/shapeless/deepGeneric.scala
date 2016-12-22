package shapeless

trait DeepGeneric[T] {
  type Repr <: HList

  def to(t: T): Repr
  def from(t: Repr): T
}

object DeepGeneric {
  type Aux[T, Repr0] = DeepGeneric[T] { type Repr = Repr0 }

  implicit val hnilDeepGeneric: DeepGeneric.Aux[HNil, HNil] = new DeepGeneric[HNil] {
    type Repr = HNil
    def to(h: HNil): HNil = h
    def from(h: HNil): HNil = h
  }

  implicit def hlistDeepGeneric[H, T <: HList, TR <: HList, HR <: HList](
    implicit hgen: Lazy[DeepGeneric.Aux[H, HR]],
      tgen: DeepGeneric.Aux[T, TR]
  ): DeepGeneric.Aux[H :: T, HR :: TR] = new DeepGeneric[H :: T] {
    type Repr = HR :: TR
    def to(hlist: H :: T): Repr = hlist match {
      case h :: t => hgen.value.to(h) :: tgen.to(t)
    }
    def from(r: Repr): H :: T = r match {
      case h :: t => hgen.value.from(h) :: tgen.from(t)
    }
  }

  implicit def genericDeepGeneric[A, R <: HList, DR <: HList](implicit g: Generic.Aux[A, R],
    dg: DeepGeneric.Aux[R, DR]): DeepGeneric.Aux[A, DR] = new DeepGeneric[A] {
    type Repr = DR
    def to(a: A): Repr = dg.to(g.to(a))
    def from(r: Repr): A = g.from(dg.from(r))
  }

  def baseDeepGeneric[A]: DeepGeneric.Aux[A, A :: HNil] = new DeepGeneric[A] {
    type Repr = A :: HNil
    def to(a: A): Repr = a :: HNil
    def from(r: Repr): A = r match { case a :: HNil => a }
  }

  implicit val intDeepGeneric: DeepGeneric.Aux[Int, Int :: HNil] = baseDeepGeneric[Int]
  implicit val longDeepGeneric: DeepGeneric.Aux[Long, Long :: HNil] = baseDeepGeneric[Long]
  implicit val booleanDeepGeneric: DeepGeneric.Aux[Boolean, Boolean :: HNil] = baseDeepGeneric[Boolean]
  implicit val stringDeepGeneric: DeepGeneric.Aux[String, String :: HNil] = baseDeepGeneric[String]
}

package shapeless

/**
 * Represent a `T` made of implicits.
 *
 * E.g. Looking for an implicit `Implicit[A :: B :: HNil]` will
 * trigger implicit lookups for `A` and `B`, and return these
 * wrapped in an `Implicit[A :: B :: HNil]`.
 *
 * Looking for an implicit `Implicit[A :+: B :+: CNil]` will
 * trigger first an implicit lookup for a `A`. If it fails,
 * an implicit `B` will be looked up. The result of these lookup
 * will be wrapped in an `Implicit[A :+: B :+: CNil]`, whose
 * value will be either a `A` (if an implicit `A` was found),
 * or a `B` (if no implicit `A` was found, but an implicit `B` was).
 *
 * `T` can also be a product or an ADT. Its generic counterpart will
 * looked upon as described above, then converted to a `T`.
 */
trait Implicit[T] {
  def value: T
}

trait LowPriorityImplicit {
  implicit def cconsImplicitTail[H, T <: Coproduct]
   (implicit
     t: Lazy[Implicit[T]]
   ): Implicit[H :+: T] =
    Implicit.of[H :+: T](Inr(t.value.value))
}

object Implicit extends LowPriorityImplicit {
  def apply[T](implicit impl: Implicit[T]): Implicit[T] = impl

  def of[T](t: => T): Implicit[T] =
    new Implicit[T] {
      def value = t
    }

  implicit val hnilImplicit: Implicit[HNil] =
    Implicit.of[HNil](HNil)

  implicit def hconsImplicit[H, T <: HList]
   (implicit
     h: Lazy[H],
     t: Lazy[Implicit[T]]
   ): Implicit[H :: T] =
    Implicit.of[H :: T](h.value :: t.value.value)

  implicit def cconsImplicitHead[H, T <: Coproduct]
   (implicit
     h: Lazy[H]
   ): Implicit[H :+: T] =
    Implicit.of[H :+: T](Inl(h.value))

  implicit def genericImplicit[F, G]
   (implicit
     gen: Generic.Aux[F, G],
     impl: Lazy[Implicit[G]]
   ): Implicit[F] =
    Implicit.of[F](gen.from(impl.value.value))
}


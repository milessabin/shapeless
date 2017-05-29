package shapeless

/**
  * A type class that is isomorphic to the kind it abstracts over for every kind `*`.
  * A type class inheriting this behavior can automatically abstract over the `product`
  * operation. See ProductTypeClassIsomorphismCompanion.
  */
trait TypeClassIsomorphism[C[_]] extends Serializable { // an isomorphism between T and C[T]
  def point[T](t: T): C[T]
  def unwrap[T](ct: C[T]): T
}

trait ProductTypeClassIsomorphismCompanion[C[_]] extends Serializable with ProductTypeClassCompanion[C] with TypeClassIsomorphism[C] {
  trait typeClassTrait extends Serializable with ProductTypeClass[C] {
    def product[H, T <: HList](ch: C[H], ct: C[T]): C[H :: T] = point(unwrap(ch) :: unwrap(ct))
    def emptyProduct: C[HNil] = point(HNil)
    def project[F, G](instance: => C[G], to: (F) => G, from: (G) => F): C[F] = point(from(unwrap(instance)))
  }

  object typeClass extends typeClassTrait
}

/**
  * A type class that can be created from a single function `T => T` for any type `T`.
  * Given an instance of the type class and a value of type `T`, the type class can be applied
  * on the value to return a type `T`
  * A type class inheriting this behavior can automatically abstract over the `product`
  * operation. See ProductTypeClassApplyInstanceCompanion.
  */
trait TypeClassApplyInstance[C[_]] extends Serializable {
  def point[T](f: T => T): C[T]
  def applyInstance[T](instance: => C[T], t: T): T
}

trait ProductTypeClassApplyInstanceCompanion[C[_]] extends Serializable with ProductTypeClassCompanion[C] with TypeClassApplyInstance[C] {
  trait typeClassTrait extends Serializable with ProductTypeClass[C] {
    def product[H, T <: HList](ch: C[H], ct: C[T]): C[H :: T] = point(hl => applyInstance(ch, hl.head) :: applyInstance(ct, hl.tail))
    def emptyProduct: C[HNil] = point(identity)
    def project[F, G](instance: => C[G], to: (F) => G, from: (G) => F): C[F] = point(f => from(applyInstance(instance, to(f))))
  }

  object typeClass extends typeClassTrait
}

/**
  * A type class that can be created from a single function `T => G` for any types `T` and `G`.
  * Given an instance of the type class and a value of type `T`, the type class can be applied
  * on the value to return a type `G`
  * A type class inheriting this behavior can automatically abstract over the `coproduct`
  * operation. See CoproductTypeClassApplyInstanceCompanion.
  */
trait CoproductTypeClassApplyInstance[C[_], G] extends Serializable {
  def point[T](f: T => G): C[T]
  def applyInstance[T](instance: => C[T], t: T): G
}

trait CoproductTypeClassApplyInstanceCompanion[C[_], G] extends Serializable with CoproductTypeClassCompanion[C] with CoproductTypeClassApplyInstance[C, G] {
  trait typeClassTrait extends Serializable with CoproductTypeClass[C] {
    def coproduct[L, R <: Coproduct](cl: => C[L], cr: => C[R]): C[L :+: R] = point {
      case Inl(l) => applyInstance(cl, l)
      case Inr(r) => applyInstance(cr, r)
    }
    def emptyCoproduct: C[CNil] = point(_ => ???)
  }

  object typeClass extends typeClassTrait
}

/**
  * A type class that can be created from a single function `T => G` for any types `T` and `G`,
  * where G is also a Monoid.
  * Given an instance of the type class and a value of type `T`, the type class can be applied
  * on the value to return a type `G`
  * A type class inheriting this behavior can automatically abstract over the `product` and `coproduct`
  * operations. See TypeClassCompanion1.
  */
trait Companion1[C[_], G] extends Serializable {
  def point[T](f: T => G): C[T]
  def applyInstance[T](instance: => C[T], t: T): G
  def zero: G
  def append(left: G, right: G): G
  implicit class GOps(left: G) {
    def |+|(right: G) = append(left, right)
  }
}

trait TypeClassCompanion1[C[_], G] extends Serializable with TypeClassCompanion[C] with Companion1[C, G] {
  trait typeClassTrait extends Serializable with TypeClass[C] {
    def emptyCoproduct: C[CNil] = point(_ => zero)
    def coproduct[L, R <: Coproduct](cl: => C[L], cr: => C[R]): C[L :+: R] = point {
      case Inl(l) => applyInstance(cl, l)
      case Inr(r) => applyInstance(cr, r)
    }
    def product[H, T <: HList](ch: C[H], ct: C[T]): C[H :: T] =
      point(hlist => applyInstance(ch, hlist.head) |+| applyInstance(ct, hlist.tail))
    def emptyProduct: C[HNil] = point(_ => zero)
    def project[F, G](instance: => C[G], to: F => G, from: G => F): C[F] = point((t: F) => applyInstance(instance, to(t)))
  }

  object typeClass extends typeClassTrait
}

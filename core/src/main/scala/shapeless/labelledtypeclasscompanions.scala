package shapeless

/**
  * A type class that is isomorphic to the kind it abstracts over.
  * Refines TypeClassIsomorphism with the addition of runtime `String` labels
  * corresponding to the names of the product elements.
  * A type class inheriting this behavior can automatically abstract over the `product`
  * operation. See LabelledProductTypeClassIsomorphismCompanion.
  */
trait LabelledTypeClassIsomorphism[C[_]] extends Serializable { // an isomorphism between T and C[T]
  def point[T](t: T): C[T]
  def unwrap[T](ct: C[T]): T
  def unwrap[T](name: String, ct: C[T]): T
}

trait LabelledProductTypeClassIsomorphismCompanion[C[_]] extends Serializable with LabelledProductTypeClassCompanion[C] with LabelledTypeClassIsomorphism[C] {
  trait typeClassTrait extends Serializable with LabelledProductTypeClass[C] {
    def product[H, T <: HList](name: String, ch: C[H], ct: C[T]): C[H :: T] = point(unwrap(name, ch) :: unwrap(ct))
    def emptyProduct: C[HNil] = point(HNil)
    def project[F, G](instance: => C[G], to: (F) => G, from: (G) => F): C[F] = point(from(unwrap(instance)))
  }

  object typeClass extends typeClassTrait
}

/**
  * A type class that can be created from a single function `T => T` for any type `T`.
  * Given an instance of the type class and a value of type `T`, the type class can be applied
  * on the value to return a type `T`
  * Refines TypeClassApplyInstance with the addition of runtime `String` labels
  * corresponding to the names of the product elements.
  * A type class inheriting this behavior can automatically abstract over the `product`
  * operation. See LabelledProductTypeClassApplyInstanceCompanion.
  */
trait LabelledTypeClassApplyInstance[C[_]] {
  def point[T](f: T => T): C[T]
  def applyInstance[T](instance: => C[T], t: T): T
  def applyInstance[T](name: String, instance: => C[T], t: T): T
}

trait LabelledProductTypeClassApplyInstanceCompanion[C[_]] extends Serializable with LabelledProductTypeClassCompanion[C] with LabelledTypeClassApplyInstance[C] {
  trait typeClassTrait extends Serializable with LabelledProductTypeClass[C] {
    def product[H, T <: HList](name: String, ch: C[H], ct: C[T]): C[H :: T] = point(hl => applyInstance(name, ch, hl.head) :: applyInstance(ct, hl.tail))
    def emptyProduct: C[HNil] = point(identity)
    def project[F, G](instance: => C[G], to: (F) => G, from: (G) => F): C[F] = point(f => from(applyInstance(instance, to(f))))
  }

  object typeClass extends typeClassTrait
}

/**
  * A type class that can be created from a single function `T => G` for any types `T` and `G`.
  * Given an instance of the type class and a value of type `T`, the type class can be applied
  * on the value to return a type `G`
  * Refines CoproductTypeClassApplyInstance with the addition of runtime `String` labels
  * corresponding to the names of the product elements.
  * A type class inheriting this behavior can automatically abstract over the `coproduct`
  * operation. See LabelledCoproductTypeClassApplyInstanceCompanion.
  */
trait LabelledCoproductTypeClassApplyInstance[C[_], G] extends Serializable {
  def point[T](f: T => G): C[T]
  def applyInstance[T](instance: => C[T], t: T): G
  def applyInstance[T](name: String, instance: => C[T], t: T): G
}

trait LabelledCoproductTypeClassApplyInstanceCompanion[C[_], G] extends Serializable with LabelledCoproductTypeClassCompanion[C] with LabelledCoproductTypeClassApplyInstance[C, G] {
  trait typeClassTrait extends Serializable with LabelledCoproductTypeClass[C] {
    def coproduct[L, R <: Coproduct](name: String, cl: => C[L], cr: => C[R]): C[L :+: R] = point {
      case Inl(l) => applyInstance(name, cl, l)
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
  * Refines Companion1 with the addition of runtime `String` labels
  * corresponding to the names of the product elements.
  * A type class inheriting this behavior can automatically abstract over the `product` and `coproduct`
  * operations. See LabelledTypeClassCompanion1.
  */
trait LabelledCompanion1[C[_], G] extends Serializable {
  def point[T](f: T => G): C[T]
  def applyInstance[T](instance: => C[T], t: T): G
  def applyInstanceProduct[T](name: String, instance: => C[T], t: T): G
  def applyInstanceCoproduct[T](name: String, instance: => C[T], t: T): G
  def zero: G
  def append(left: G, right: G): G
  implicit class GOps(left: G) {
    def |+|(right: G) = append(left, right)
  }
}

trait LabelledTypeClassCompanion1[C[_], G] extends Serializable with LabelledTypeClassCompanion[C] with LabelledCompanion1[C, G] {
  trait typeClassTrait extends Serializable with LabelledTypeClass[C] {
    def emptyCoproduct: C[CNil] = point(_ => zero)
    def coproduct[L, R <: Coproduct](name: String, cl: => C[L], cr: => C[R]): C[L :+: R] = point {
      case Inl(l) => applyInstanceCoproduct(name, cl, l)
      case Inr(r) => applyInstance(cr, r)
    }
    def product[H, T <: HList](name: String, ch: C[H], ct: C[T]): C[H :: T] =
      point(hlist => applyInstanceProduct(name, ch, hlist.head) |+| applyInstance(ct, hlist.tail))
    def emptyProduct: C[HNil] = point(_ => zero)
    def project[F, G](instance: => C[G], to: F => G, from: G => F): C[F] = point((t: F) => applyInstance(instance, to(t)))
  }

  object typeClass extends typeClassTrait
}

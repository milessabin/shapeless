/*
 * Copyright (c) 2014 Miles Sabin
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package shapeless

import scala.reflect.ClassTag

import record._
import ops.hlist.{ Length, Tupler }
import ops.nat.ToInt
import ops.record.Merger

trait CaseClassFacet {
  type C
}

trait ProductISOFacet extends CaseClassFacet {
  trait ProductISOOps {
    type Repr <: HList
    type P <: Product
    val gen: Generic.Aux[C, Repr]
    val pgen: Generic.Aux[P, Repr]

    def toProduct(c: C): P = pgen.from(gen.to(c))
    def fromProduct(p: P): C = gen.from(pgen.to(p))
  }

  val ops: ProductISOOps
}

trait ApplyUnapplyFacet extends ProductISOFacet {
  trait ApplyUnapplyOps extends ProductISOOps {
    def apply(p: P): C = fromProduct(p)

    def unapply(c: C): Option[P] = Some(toProduct(c))
  }

  val ops: ApplyUnapplyOps

  trait ApplyUnapplyCompanion {
    @nonGeneric def apply(elems: ops.P): C = ops.apply(elems)
    @nonGeneric def unapply(s: C): Option[ops.P] = ops.unapply(s)
  }
}

trait ProductFacet extends ProductISOFacet {
  trait ProductOps extends ProductISOOps {
    def productElement(c: C, n: Int): Any = toProduct(c).productElement(n)

    def productIterator(c: C): Iterator[Any] = toProduct(c).productIterator

    def productPrefix: String

    def productArity: Int
  }

  val ops: ProductOps

  trait ProductMethods { self: C =>
    def productElement(n: Int): Any = ops.productElement(this, n)

    def productIterator: Iterator[Any] = ops.productIterator(this)

    def productPrefix: String = ops.productPrefix

    def productArity: Int = ops.productArity
  }
}

trait PolymorphicEqualityFacet extends ProductISOFacet {
  trait PolymorphicEqualityOps extends ProductISOOps {
    val typ: Typeable[C]

    def canEqual(c: C, other: Any): Boolean = typ.cast(other).isDefined

    def equals(c: C, other: Any): Boolean =
      (c.asInstanceOf[AnyRef] eq other.asInstanceOf[AnyRef]) ||
        typ.cast(other).exists { that =>
          (toProduct(c) == toProduct(that)) && canEqual(that, c)
        }

    def hashCode(c: C): Int = toProduct(c).hashCode
  }

  val ops: PolymorphicEqualityOps

  trait PolymorphicEqualityMethods { self: C =>
    override def equals(other: Any): Boolean = ops.equals(this, other)

    override def hashCode: Int = ops.hashCode(this)
  }
}

trait CopyFacet extends CaseClassFacet {
  trait CopyOps {
    type LRepr <: HList
    type CopyMerger[R <: HList] = Merger.Aux[LRepr, R, LRepr]

    val lgen: LabelledGeneric.Aux[C, LRepr]

    def copy[R <: HList](c: C, rec: R)(implicit merger: CopyMerger[R]): C =
      lgen.from(lgen.to(c).merge(rec))
  }

  val ops: CopyOps

  trait CopyMethods extends RecordArgs { self: C =>
    def copyRecord[R <: HList](rec: R)(implicit merger: ops.CopyMerger[R]): C = ops.copy(this, rec)
  }
}

trait ToStringFacet extends ProductFacet {
  trait ToStringOps extends ProductOps {
    def toString(c: C): String = productPrefix+toProduct(c).toString
  }

  val ops: ToStringOps

  trait ToStringMethods { self: C =>
    override def toString: String = ops.toString(this)
  }
}

trait DefaultCaseClassDefns extends
  ApplyUnapplyFacet with
  ProductFacet with
  PolymorphicEqualityFacet with
  CopyFacet with
  ToStringFacet {

  trait CaseClassOps extends
    ApplyUnapplyOps with
    ProductOps with
    PolymorphicEqualityOps with
    CopyOps with
    ToStringOps

  trait CaseClassCompanion extends
    ApplyUnapplyCompanion

  trait CaseClass extends
    ProductMethods with
    PolymorphicEqualityMethods with
    CopyMethods with
    ToStringMethods { self: C => }

  val ops: CaseClassOps

  def Ops[Repr0 <: HList, LRepr0 <: HList, P0 <: Product, N <: Nat]
    (implicit
      gen0: Generic.Aux[C, Repr0],
      lgen0: LabelledGeneric.Aux[C, LRepr0],
      len: Length.Aux[Repr0, N],
      toInt: ToInt[N],
      tup: Tupler.Aux[Repr0, P0],
      pgen0: Generic.Aux[P0, Repr0],
      typ0: Typeable[C],
      tag0: ClassTag[C]
    ) =
      new CaseClassOps {
        type Repr = Repr0
        type LRepr = LRepr0
        type P = P0
        val gen = gen0
        val lgen = lgen0
        val pgen = pgen0
        val typ = typ0
        val tag = tag0
        val productPrefix = tag0.runtimeClass.getName.split("(\\.|\\$)").last
        val productArity = toInt()
      }
}

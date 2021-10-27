/*
 * Copyright (c) 2012-18 Lars Hupel, Miles Sabin
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

import scala.deriving._

trait GenericScalaCompat extends GenericScalaCompatLowPriority {

  given Generic.Aux[Unit, HNil] = new Generic[Unit] {
    override type Repr = HNil

    override def to(t: Unit): Repr = HNil

    override def from(r: Repr): Unit = ()
  }

  given[A <: AnyRef & Singleton](using v: ValueOf[A]): Generic.Aux[A, HNil] = new Generic[A] {
    override type Repr = HNil

    override def to(t: A): Repr = HNil

    override def from(r: Repr): A = v.value
  }
}

trait GenericScalaCompatLowPriority {

  transparent inline given materializeProduct[T <: Product](
    using m: scala.deriving.Mirror.ProductOf[T]
  ): Generic.Aux[T, HList.TupleToHList[m.MirroredElemTypes]] =
    new Generic[T] {
      override type Repr = HList.TupleToHList[m.MirroredElemTypes]

      override def to(t: T): Repr = HList.tupleToHList(scala.Tuple.fromProductTyped(t))

      override def from(r: Repr): T = m.fromProduct(HList.hListToTuple(r))
    }

  transparent inline given materializeSum[T](
    using m: scala.deriving.Mirror.SumOf[T],
    ev: scala.Tuple.Union[Coproduct.CoproductToTuple[Coproduct.TupleToCoproduct[m.MirroredElemTypes]]] <:< T,
  ): Generic.Aux[T, Coproduct.TupleToCoproduct[m.MirroredElemTypes]] =
    new Generic[T] {
      override type Repr = Coproduct.TupleToCoproduct[m.MirroredElemTypes]

      override def to(t: T): Repr = Coproduct.coproductFromOrdinal(t.asInstanceOf[scala.Tuple.Union[m.MirroredElemTypes]], m.ordinal(t))

      override def from(r: Repr): T = ev(Coproduct.extractCoproduct(r))
    }
}

trait LabelledGenericScalaCompat {

  type MakeFieldsProduct[Types <: scala.Tuple, Labels <: scala.Tuple] <: HList = (Types, Labels) match {
    case (EmptyTuple, EmptyTuple)        => HNil
    case (tpe *: types, label *: labels) => labelled.FieldType[label, tpe] :: MakeFieldsProduct[types, labels]
  }
  
  type MakeFieldsCoproduct[Types <: scala.Tuple, Labels <: scala.Tuple] <: Coproduct = (Types, Labels) match {
    case (EmptyTuple, EmptyTuple)        => CNil
    case (tpe *: types, label *: labels) => labelled.FieldType[label, tpe] :+: MakeFieldsCoproduct[types, labels]
  }

  transparent inline given materializeProduct[T <: Product](
    using m: scala.deriving.Mirror.ProductOf[T]
  ): LabelledGeneric.Aux[T, MakeFieldsProduct[m.MirroredElemTypes, m.MirroredElemLabels]] =
    LabelledGeneric.unsafeInstance(Generic.materializeProduct)

  transparent inline given materializeSum[T](
    using m: scala.deriving.Mirror.SumOf[T],
    ev: scala.Tuple.Union[Coproduct.CoproductToTuple[Coproduct.TupleToCoproduct[m.MirroredElemTypes]]] <:< T,
  ): LabelledGeneric.Aux[T, MakeFieldsCoproduct[m.MirroredElemTypes, m.MirroredElemLabels]] =
    LabelledGeneric.unsafeInstance(Generic.materializeSum)
}

trait IsTupleScalaCompat {
  given[T <: scala.Tuple]: IsTuple[T] = new IsTuple[T]
}

trait HasProductGenericScalaCompat {
  given [T](using Mirror.ProductOf[T]): HasProductGeneric[T] = new HasProductGeneric[T]
}

trait HasCoproductGenericScalaCompat {
  given [T](using Mirror.SumOf[T]): HasCoproductGeneric[T] = new HasCoproductGeneric[T]
}

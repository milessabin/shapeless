/*
 * Copyright (c) 2012-16 Miles Sabin
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

package shapeless.examples

/**
 * We need to define our example algebraic data type in a separate object in
 * order for the LabelledGeneric instance to be found.
 *
 * @author Travis Brown
 */
object ADTPartitionExampleTypes {
  /**
   * An example type with two constructors.
   */
  sealed trait Fruit

  case class Apple(id: Int, sweetness: Int) extends Fruit
  case class Pear(id: Int, color: String) extends Fruit
}

/**
 * Partitioning lists of some algebraic data type by that type's constructors.
 *
 * See this blog post for additional discussion:
 * http://meta.plasm.us/posts/2014/06/14/partitioning-by-constructor/
 *
 * @author Travis Brown
 */
object ADTPartitionExample extends App {
  import shapeless._
  import labelled._
  import ops.hlist.Tupler
  import record._
  import test._

  /**
   * A type class that partitions a list of coproduct items into an HList of
   * lists of the coproduct alternatives.
   */
  trait Partitioner[C <: Coproduct] extends DepFn1[List[C]] {
    type Out <: HList
  }

  object Partitioner {
    type Aux[C <: Coproduct, Out0 <: HList] = Partitioner[C] {
      type Out = Out0
    }

    implicit def cnilPartitioner: Aux[CNil, HNil] = new Partitioner[CNil] {
      type Out = HNil

      def apply(c: List[CNil]): Out = HNil
    }

    implicit def cpPartitioner[K, H, T <: Coproduct, OutT <: HList]
      (implicit cp: Aux[T, OutT]): Aux[FieldType[K, H] :+: T, FieldType[K, List[H]] :: OutT] =
        new Partitioner[FieldType[K, H] :+: T] {
          type Out = FieldType[K, List[H]] :: OutT

          def apply(c: List[FieldType[K, H] :+: T]): Out =
            field[K](c.collect { case Inl(h) => h: H }) :: cp(c.collect { case Inr(t) => t })
        }
  }

  /**
   * Partition a list into a tuple of lists for each constructor.
   */
  def partitionTuple[A, C <: Coproduct, Out <: HList](as: List[A])
    (implicit
      gen: LabelledGeneric.Aux[A, C],
      partitioner: Partitioner.Aux[C, Out],
      tupler: Tupler[Out]
    ) = tupler(partitioner(as.map(gen.to)))

  /**
   * Partition a list into a record of lists for each constructor.
   */
  def partitionRecord[A, C <: Coproduct, Out <: HList](as: List[A])
    (implicit gen: LabelledGeneric.Aux[A, C], partitioner: Partitioner.Aux[C, Out]): Out =
      partitioner(as.map(gen.to))

  import ADTPartitionExampleTypes._

  // Some example data.
  val fruits: List[Fruit] = List(
    Apple(1, 10),
    Pear(2, "red"),
    Pear(3, "green"),
    Apple(4, 6),
    Pear(5, "purple")
  )

  // The expected partition.
  val expectedApples: List[Apple] = List(Apple(1, 10), Apple(4, 6))
  val expectedPears: List[Pear] = List(Pear(2, "red"), Pear(3, "green"), Pear(5, "purple"))

  // Partition the list into a tuple of lists.
  val (apples, pears) = partitionTuple(fruits)

  // Confirm that the types are as specific as possible and in the right order.
  typed[List[Apple]](apples)
  typed[List[Pear]](pears)

  assert(apples == expectedApples)
  assert(pears == expectedPears)

  // Partition the list into a record of lists.
  val basket = partitionRecord(fruits)

  // Confirm that expected record values are present (and not unexpected ones).
  typed[List[Apple]](basket(Symbol("Apple")))
  typed[List[Pear]](basket(Symbol("Pear")))
  illTyped("""basket(Symbol("Burger"))""")

  assert(basket(Symbol("Apple")) == expectedApples)
  assert(basket(Symbol("Pear")) == expectedPears)
}

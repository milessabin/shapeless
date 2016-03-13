package shapeless

import org.junit.Test

import shapeless.test._
import shapeless.union.Union

import scala.collection.immutable.{ LinearSeq, Queue }

class SubTypeTests {

  @Test
  def likeStandardSubType {
    <::<[String, AnyRef]
  }

  @Test
  def coproducts {
    <::<[CNil, AnyRef]
    <::<[String :+: Seq[Int] :+: Option[Double] :+: CNil, AnyRef]
    <::<[List[Double] :+: Vector[Double] :+: IndexedSeq[Double] :+: CNil, Seq[Double]]
    <::<[Union.`'s -> String, 'seq -> Seq[Int], 'opt -> Option[Double]`.T, AnyRef]
    <::<[Union.`'list -> List[Double], 'vector -> Vector[Double], 'is -> IndexedSeq[Double]`.T, Seq[Double]]

    <::<[(Int :+: CNil) :+: (String :+: CNil) :+: CNil, _ :+: CNil]
    <::<[(String :+: CNil) :+: (String :+: CNil) :+: CNil, String :+: CNil]
    <::<[(String :+: CNil) :+: (String :+: CNil) :+: CNil, String]
  }

  @Test
  def recursive {
    type C1 = List[Double] :+: Vector[Double] :+: IndexedSeq[Double] :+: CNil
    type C2 = Queue[Double] :+: LinearSeq[Double] :+: Stream[Double] :+: CNil
    type C12 = C1 :+: C2 :+: CNil

    <::<[C12, Seq[Double]]
    <::<[C1 :+: Queue[Double] :+: LinearSeq[Double] :+: Stream[Double] :+: CNil, Seq[Double]]
    <::<[Queue[Double] :+: LinearSeq[Double] :+: C1 :+: Stream[Double] :+: CNil, Seq[Double]]

    <::<[C12 :+: C1 :+: Stream[Double] :+: CNil, Seq[Double]]
    <::<[Stream[Double] :+: C1 :+: C12 :+: CNil, Seq[Double]]
  }

}

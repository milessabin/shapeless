package shapeless

import org.junit.Test

class SerializationTestsScala2 {
  import SerializationTestDefns._

  @Test
  def testFunctor: Unit = {
    assertSerializableBeforeAfter(Functor[Some])(_.map(Some(2))(_.toString))
    assertSerializableBeforeAfter(Functor[Option])(_.map(Option(2))(_.toString))
    assertSerializableBeforeAfter(Functor[Tree])(_.map(Leaf(2))(_.toString))
    assertSerializableBeforeAfter(Functor[List])(_.map(List(2))(_.toString))
  }
}

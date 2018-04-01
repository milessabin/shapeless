package shapeless

import org.junit.Test
import org.junit.Assert.assertArrayEquals
import testutil._

import syntax.std.product._
import record._

class ProductTests {

  case object Empty
  case class EmptyCC()
  case class Foo(i: Int, s: String)
  case class Bar(b: Boolean, f: Foo)
  case class Baz(s: String, f: Foo)

  def equalInferredTypes[A,B](a: A, b: B)(implicit eq: A =:= B): Unit = {}

  @Test
  def testToTuple = {
    {
      // FIXME: should work (needs changes in GenericMacros?)
      // Empty.toTuple
    }
    
    {
      val e = EmptyCC()
      val el = e.toTuple
      equalInferredTypes((), el)
    }

    val foo = Foo(1, "b")
    val bar = Bar(true, foo)
    
    {
      val fooT = foo.toTuple
      val expectedFooT = (1, "b")
      equalInferredTypes(expectedFooT, fooT)
      assertTypedEquals(expectedFooT, fooT)
    }
    
    {
      val barT = bar.toTuple
      val expectedBarT = (true, foo)
      equalInferredTypes(expectedBarT, barT)
      assertTypedEquals(expectedBarT, barT)
    }
    
    // With explicit type arguments, >: or =:= to the inferred ones respectively

    {
      val fooT = foo.toTuple[(AnyVal, String)]
      val expectedFooT = (1: AnyVal, "b")
      equalInferredTypes(expectedFooT, fooT)
      assertTypedEquals(expectedFooT, fooT)
    }

    {
      val barT = bar.toTuple[(Boolean, Foo)]
      val expectedBarT = (true, foo)
      equalInferredTypes(expectedBarT, barT)
      assertTypedEquals(expectedBarT, barT)
    }
  }

  @Test
  def testToHList = {
    {
      // FIXME: should work (needs changes in GenericMacros?)
      // Empty.toHList 
    }
    
    {
      val e = EmptyCC()
      val el = e.toHList
      equalInferredTypes(HNil: HNil, el)
    }

    val foo = Foo(1, "b")
    val bar = Bar(true, foo)
    
    {
      val fooL = foo.toHList
      val expectedFooL = 1 :: "b" :: HNil
      equalInferredTypes(expectedFooL, fooL)
      assertTypedEquals(expectedFooL, fooL)
    }
    
    {
      val barL = bar.toHList
      val expectedBarL = true :: foo :: HNil
      equalInferredTypes(expectedBarL, barL)
      assertTypedEquals(expectedBarL, barL)
    }
    
    // With explicit type arguments, >: or =:= to the inferred ones respectively

    {
      val fooL = foo.toHList[AnyVal :: String :: HNil]
      val expectedFooL = (1: AnyVal) :: "b" :: HNil
      equalInferredTypes(expectedFooL, fooL)
      assertTypedEquals(expectedFooL, fooL)
    }

    {
      val barL = bar.toHList[Boolean :: Foo :: HNil]
      val expectedBarL = true :: foo :: HNil
      equalInferredTypes(expectedBarL, barL)
      assertTypedEquals(expectedBarL, barL)
    }
  }

  @Test
  def testToRecord = {
    {
      // FIXME: should work (needs changes in GenericMacros?)
      // Empty.toRecord 
    }

    {
      val e = EmptyCC()
      val el = e.toRecord
      equalInferredTypes(HNil: HNil, el)
    }

    val foo = Foo(1, "b")
    val bar = Bar(true, foo)

    {
      val fooL = foo.toRecord
      val expectedFooL = Record(i = 1, s = "b")
      equalInferredTypes(expectedFooL, fooL)
      assertTypedEquals(expectedFooL, fooL)
    }

    {
      val barL = bar.toRecord
      val expectedBarL = Record(b = true, f = foo)
      equalInferredTypes(expectedBarL, barL)
      assertTypedEquals(expectedBarL, barL)
    }
    
    // With explicit type arguments, >: or =:= to the inferred ones respectively

    {
      val fooL = foo.toRecord[Record.`'i -> AnyVal, 's -> String`.T]
      val expectedFooL = Record(i=1: AnyVal, s="b")
      equalInferredTypes(expectedFooL, fooL)
      assertTypedEquals(expectedFooL, fooL)
    }

    {
      val barL = bar.toRecord[Record.`'b -> Boolean, 'f -> Foo`.T]
      val expectedBarL = Record(b=true, f=foo)
      equalInferredTypes(expectedBarL, barL)
      assertTypedEquals(expectedBarL, barL)
    }
  }

  @Test
  def testToTraversable: Unit = {
    def assertArrayEquals0[T](a: Array[T], b: Array[T]) =
      assertArrayEquals(a.asInstanceOf[Array[Object]], b.asInstanceOf[Array[Object]])

    {
      // FIXME: should work (needs changes in GenericMacros?)
      // val l = Empty.to[List]
      // assertTypedEquals(List.empty[Nothing], l)
    }


    val e = EmptyCC()

    {
      val l = e.to[List]
      val expected = List.empty[Nothing]
      equalInferredTypes(expected, l)
      assertTypedEquals(expected, l)
    }

    {
      val a = e.to[Array]
      val expected = Array.empty[Nothing]
      equalInferredTypes(expected, a)
      assertArrayEquals0(expected, a)
    }

    val foo = Foo(1, "b")

    {
      val l = foo.to[List]
      val expected = List(1, "b")
      equalInferredTypes(expected, l)
      assertTypedEquals(expected, l)
    }

    {
      val a = foo.to[Array]
      val expected = Array(1, "b")
      equalInferredTypes(expected, a)
      assertArrayEquals0(expected, a)
    }
    
    val baz = Baz("a", foo)

    {
      val l = baz.to[List]
      val expected = List("a", foo)
      equalInferredTypes(expected, l)
      assertTypedEquals(expected, l)
    }

    {
      val a = baz.to[Array]
      val expected = Array("a", foo)
      equalInferredTypes(expected, a)
      assertArrayEquals0(expected, a)
    }
  }
  
  @Test
  def testToSized: Unit = {
    def assertArrayEquals0[T](a: Array[T], b: Array[T]) =
      assertArrayEquals(a.asInstanceOf[Array[Object]], b.asInstanceOf[Array[Object]])

    {
      // FIXME: should work (needs changes in GenericMacros?)
      // val l = Empty.toSized[List]
      // assertTypedEquals(Sized[List](), l)
    }


    val e = EmptyCC()

    {
      val l = e.toSized[List]
      val expected = Sized[List]()
      equalInferredTypes(expected, l)
      assertTypedEquals(expected.unsized, l.unsized)
    }

    {
      val a = e.toSized[Array]
      val expected = Sized[Array]()
      equalInferredTypes(expected, a)
      assertArrayEquals0(expected.unsized, a.unsized)
    }

    val foo = Foo(1, "b")

    {
      val l = foo.toSized[List]
      val expected = Sized[List](1, "b")
      equalInferredTypes(expected, l)
      assertTypedEquals(expected.unsized, l.unsized)
    }

    {
      val a = foo.toSized[Array]
      val expected = Sized[Array](1, "b")
      equalInferredTypes(expected, a)
      assertArrayEquals0(expected.unsized, a.unsized)
    }

    val baz = Baz("a", foo)

    {
      val l = baz.toSized[List]
      val expected = Sized[List]("a", foo)
      equalInferredTypes(expected, l)
      assertTypedEquals(expected.unsized, l.unsized)
    }

    {
      val a = baz.toSized[Array]
      val expected = Sized[Array]("a", foo)
      equalInferredTypes(expected, a)
      assertArrayEquals0(expected.unsized, a.unsized)
    }
  }
  
  @Test
  def testToMap: Unit = {
    import syntax.singleton._
    
    {
      // FIXME: should work (needs changes in GenericMacros?)
      // val m = Empty.toMap
      // assertTypedEquals(Map.empty[Any, Nothing], m)
    }


    val e = EmptyCC()

    {
      val m = e.toMap
      val expected = Map.empty[Any, Nothing]
      equalInferredTypes(expected, m)
      assertTypedEquals(expected, m)
    }

    {
      val m = e.toMap[String, Nothing]
      val expected = Map.empty[String, Nothing]
      equalInferredTypes(expected, m)
      assertTypedEquals(expected, m)
    }

    {
      val m = e.toMap[String, Int]
      val expected = Map.empty[String, Int]
      equalInferredTypes(expected, m)
      assertTypedEquals(expected, m)
    }

    val foo = Foo(1, "b")
    
    {
      val m = foo.toMap
      val expected = Map('i.narrow -> 1, 's.narrow -> "b")
      equalInferredTypes(expected, m)
      assertTypedEquals(expected, m)
    }

    {
      val m = foo.toMap[Symbol, Any]
      val expected = Map[Symbol, Any]('i -> 1, 's -> "b")
      equalInferredTypes(expected, m)
      assertTypedEquals(expected, m)
    }
  }
}

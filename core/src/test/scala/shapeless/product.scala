package shapeless

import org.junit.Test
import testutil._

import syntax.std.product._
import record._

class ProductTests {

  case object Empty
  case class EmptyCC()
  case class Foo(i: Int, s: String)
  case class Bar(b: Boolean, f: Foo)

  def equalInferredTypes[A,B](a: A, b: B)(implicit eq: A =:= B) {}

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
  
}

package shapeless

import org.junit.Test
import org.junit.Assert._

import ops.record._
import record._
import syntax.singleton._
import test._
import testutil._
import union._
import labelled.->>

class LabelledGenericTestsScala2 {
  import LabelledGenericTestsAux._

  @Test
  def testAbstractNonCC: Unit = {
    val ncca = new NonCCA(23, "foo")
    val nccb = new NonCCB(true, 2.0)
    val ancc: AbstractNonCC = ncca

    type NonCCARec = ("i" ->> Int) :: ("s" ->> String) :: HNil
    type NonCCBRec = ("b" ->> Boolean) :: ("d" ->> Double) :: HNil
    type AbsUnion = ("NonCCA" ->> NonCCA) :+: ("NonCCB" ->> NonCCB) :+: CNil

    val genA = LabelledGeneric[NonCCA]
    val genB = LabelledGeneric[NonCCB]
    val genAbs = LabelledGeneric[AbstractNonCC]

    val rA = genA.to(ncca)
    assertTypedEquals[NonCCARec]("i" ->> 23 :: "s" ->> "foo" :: HNil, rA)

    val rB = genB.to(nccb)
    assertTypedEquals[NonCCBRec]("b" ->> true :: "d" ->> 2.0 :: HNil, rB)

    val rAbs = genAbs.to(ancc)
    val injA = Coproduct[AbsUnion]("NonCCA" ->> ncca)
    assertTypedEquals[AbsUnion](injA, rAbs)

    val fA = genA.from("i" ->> 13 :: "s" ->> "bar" :: HNil)
    typed[NonCCA](fA)
    assertEquals(13, fA.i)
    assertEquals("bar", fA.s)

    val fB = genB.from("b" ->> false :: "d" ->> 3.0 :: HNil)
    typed[NonCCB](fB)
    assertEquals(false, fB.b)
    assertEquals(3.0, fB.d, Double.MinPositiveValue)

    val injB = Coproduct[AbsUnion]("NonCCB" ->> nccb)
    val fAbs = genAbs.from(injB)
    typed[AbstractNonCC](fAbs)
    assertTrue(fAbs.isInstanceOf[NonCCB])
    assertEquals(true, fAbs.asInstanceOf[NonCCB].b)
    assertEquals(2.0, fAbs.asInstanceOf[NonCCB].d, Double.MinPositiveValue)
  }

  @Test
  def testNonCCWithCompanion: Unit = {
    val nccc = NonCCWithCompanion(23, "foo")

    val rec = ("i" ->> 23) :: ("s" ->> "foo") :: HNil
    type NonCCRec = ("i" ->> Int) :: ("s" ->> String) :: HNil

    val gen = LabelledGeneric[NonCCWithCompanion]

    val r = gen.to(nccc)
    assertTypedEquals[NonCCRec](rec, r)

    val f = gen.from("i" ->> 13 :: "s" ->> "bar" :: HNil)
    typed[NonCCWithCompanion](f)
    assertEquals(13, f.i)
    assertEquals("bar", f.s)
  }

  @Test
  def testNonCCLazy: Unit = {
    lazy val (a: NonCCLazy, b: NonCCLazy, c: NonCCLazy) =
      (new NonCCLazy(c, b), new NonCCLazy(a, c), new NonCCLazy(b, a))

    val rec = "prev" ->> a :: "next" ->> c :: HNil
    type LazyRec = ("prev" ->> NonCCLazy) :: ("next" ->> NonCCLazy) :: HNil

    val gen = LabelledGeneric[NonCCLazy]

    val rB = gen.to(b)
    assertTypedEquals[LazyRec](rec, rB)

    val fD = gen.from("prev" ->> a :: "next" ->> c :: HNil)
    typed[NonCCLazy](fD)
    assertEquals(a, fD.prev)
    assertEquals(c, fD.next)
  }

  @Test
  def testShapelessTagged: Unit = {
    import ShapelessTaggedAux._

    val lgen = LabelledGeneric[Dummy]
    val s = s"${lgen from Record(i=tag[CustomTag](0))}"
    assertEquals(s, "Dummy(0)")
  }
}

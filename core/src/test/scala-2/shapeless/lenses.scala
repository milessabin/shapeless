package shapeless

import org.junit.Test
import org.junit.Assert._

import lens._, nat._, record._, syntax.singleton._, tag.@@, test._, testutil._

import lensTestDataTypes._

class OpticTestsScala2 {

  @Test
  def testLazyUnapply: Unit = {
    val g = optic[BGraph[Int]]
    val l = g.left
    val rl = g.right.left
    val rll = rl ~ l
    val rlg = rl ~ g
    val rrlv = g.right.right.left.value
    val rrrv = g.right.right.right.value
    val rrlvrrrv = rrlv ~ rrrv
    val rrrlv = g.right.right.right.left.value
    val rrrrlv = g.right.right.right.right.left.value
    val looped = rrrlv ~ rrrrlv

    val rll(a, b) = new BNode(BTerm(1), new BNode(BTerm(2), BTerm(3)))
    assertEquals(BTerm(2), a)
    assertEquals(BTerm(1), b)

    lazy val g0 @ rll(x: BTerm[Int], y: BTerm[Int]) = new BNode(BTerm(1), new BNode(BTerm(2), new BNode(x, y)))
    val rrlvrrrv(x1, y1) = g0
    assertEquals(2, x1)
    assertEquals(1, y1)

    lazy val rlg(z: BTerm[Int], g1: BGraph[Int]) = new BNode(BTerm(1), new BNode(BTerm(2), new BNode(z, g1)))

    val looped(x2, y2) = g1
    assertEquals(1, x2)
    assertEquals(2, y2)
  }
}

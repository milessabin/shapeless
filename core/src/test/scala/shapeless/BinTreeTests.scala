package shapeless

import scala.annotation.tailrec
import scala.util.Random

class BinTreeTests {
  import org.junit.Assert._
  import org.junit.Test

  @Test def testIntroL(): Unit = {
    assertEquals(Some(Node(Nil, a)), introL(a))
    assertEquals(Some(Node(Nil, Node(a, b))), introL(Node(a, b)))
  }

  @Test def testIntroR(): Unit = {
    assertEquals(Some(Node(a, Nil)), introR(a))
    assertEquals(Some(Node(Node(a, b), Nil)), introR(Node(a, b)))
  }

  @Test def testUnitL(): Unit = {
    assertEquals(Some(a), unitL(Node(Nil, a)))
    assertEquals(None, unitL(a))
    assertEquals(None, unitL(Node(a, Nil)))
  }

  @Test def testUnitR(): Unit = {
    assertEquals(Some(a), unitR(Node(a, Nil)))
    assertEquals(None, unitR(a))
    assertEquals(None, unitR(Node(Nil, a)))
  }

  @Test def testSwap(): Unit = {
    assertEquals(Some(Node(b, a)), swap(Node(a, b)))
    assertEquals(None, swap(a))
  }

  @Test def testAssocR(): Unit = {
    assertEquals(Some(Node(a, Node(b, c))), assocR(Node(Node(a, b), c)))
    assertEquals(None, assocR(a))
    assertEquals(None, assocR(Node(a, Node(b, c))))
  }

  @Test def testAssocL(): Unit = {
    assertEquals(Some(Node(Node(a, b), c)), assocL(Node(a, Node(b, c))))
  }

  @Test def testMapL(): Unit = {
    assertEquals(Some(Node(Node(b, a), c)), mapL(swap)(Node(Node(a, b), c)))
  }

  @Test def testMapR(): Unit = {
    assertEquals(Some(Node(a, Node(c, b))), mapR(swap)(Node(a, Node(b, c))))
  }

  @Test def testShift(): Unit = {
    assertEquals(Some(Node(b, Node(a, c))), shiftR(Node(Node(a, b), c)))
    assertEquals(None, shiftR(a))
    assertEquals(None, shiftR(Node(a, Node(b, c))))
  }

  @Test def testReverse(): Unit = {
    assertEquals(Some(                         Nil), reverseR(Nil))
    assertEquals(Some(                    a #: Nil), reverseR(a #: Nil))
    assertEquals(Some(               b #: a #: Nil), reverseR(a #: b #: Nil))
    assertEquals(Some(          c #: b #: a #: Nil), reverseR(a #: b #: c #: Nil))
    assertEquals(Some(     d #: c #: b #: a #: Nil), reverseR(a #: b #: c #: d #: Nil))
    assertEquals(Some(e #: d #: c #: b #: a #: Nil), reverseR(a #: b #: c #: d #: e #: Nil))

    assertEquals(Some(                         Nil), reverseL(Nil))
    assertEquals(Some(                    Nil :# a), reverseL(Nil :# a))
    assertEquals(Some(               Nil :# b :# a), reverseL(Nil :# a :# b))
    assertEquals(Some(          Nil :# c :# b :# a), reverseL(Nil :# a :# b :# c))
    assertEquals(Some(     Nil :# d :# c :# b :# a), reverseL(Nil :# a :# b :# c :# d))
    assertEquals(Some(Nil :# e :# d :# c :# b :# a), reverseL(Nil :# a :# b :# c :# d :# e))

  }

  def demoReverse(): Unit = {
    for {
      s1 ← introR trace a #: b #: c #: Nil
      s2 ← shiftR trace s1
      s3 ← shiftR trace s2
      s4 ← shiftR trace s3
      s3 ← unitL  trace s4
    } yield ()

    for {
      s1 ← introL trace Nil :# a :# b :# c
      s2 ← shiftL trace s1
      s3 ← shiftL trace s2
      s4 ← shiftL trace s3
      s5 ← unitR  trace s4
    } yield ()

    findRun(a :# b :# Nil,                               b :# a :# Nil)
    findRun(a :# b :# c :# Nil,                     c :# b :# a :# Nil)
    findRun(a :# b :# c :# d :# Nil,           d :# c :# b :# a :# Nil)
    findRun(a :# b :# c :# d :# e :# Nil, e :# d :# c :# b :# a :# Nil)
  }

  @Test def testSeveral(): Unit = {
    val List(s, i, d) = List('s', 'i', 'd').map(Leaf)

    val op = mapL(swap) andThen assocR andThen swap

    assertEquals(Some(Node(Node(s, Nil), Node(i, Node(d, i)))), op(Node(Node(s, Node(i, Node(d, i))), Nil)))


    val start = Node(i, Node(s, Node(d, Node(i, Nil))))

    val op2 = introR andThen mapR(introR) andThen mapL(swap) andThen assocR andThen swap andThen mapL(assocL) andThen
      swap andThen mapR(mapR(introL)) andThen mapL(swap) andThen assocR andThen swap

    print(op2(start))
  }

  private val List(a, b, c, d, e) = "abcde".toList.map(Leaf)

  sealed trait BinTree {
    def #:(right: BinTree): BinTree = Node(right, this)
    def :#(left: BinTree): BinTree = Node(this, left)
  }

  case object Nil extends BinTree {
    override def toString: String = "0"
  }

  case class Leaf(value: Char) extends BinTree {
    override def toString: String = value.toString
  }

  case class Node(left: BinTree, right: BinTree) extends BinTree {
    override def toString: String = s"($left, $right)"
  }

  object Op {
    def apply(name0: String)(pf0: PartialFunction[BinTree, BinTree]): Op = new Op {
      val name = name0
      val pf = pf0
    }

    class AndThen(before: Op, after: Op) extends Op {
      val name = s"${before.name} >>> ${after.name}"

      val pf: PartialFunction[BinTree, BinTree] = {
        case tree if before(tree).flatMap(after(_)).isDefined ⇒
          before(tree).flatMap(after(_)).get
      }
    }

    class RepeatUntil(op: Op, p: BinTree ⇒ Boolean) extends Op {
      val name: String = s"repeatUntil(${op.name})"

      val pf: PartialFunction[BinTree, BinTree] = { case tree ⇒ recurse(tree) }

      @tailrec private def recurse(current: BinTree): BinTree =
        if (p(current)) current else recurse(op(current).getOrElse(sys.error {
          s"${op.name} $current : Unexpected None result"
        }))
    }

    class Named(op: Op, val name: String) extends Op {
      val pf = op.pf
    }
  }

  trait Op {
    def name: String
    def pf: PartialFunction[BinTree, BinTree]

    def trace(tree: BinTree): Option[BinTree] = {
      val result = apply(tree)

      println(s"$name ($tree) = $result")

      result
    }

    def apply(tree: BinTree): Option[BinTree] = pf.lift(tree)
    def andThen(after: Op): Op = new Op.AndThen(this, after)
    def repeatUntil(p: BinTree ⇒ Boolean): Op = new Op.RepeatUntil(this, p)
    def named(name: String) = new Op.Named(this, name)

    override def toString: String = name.padTo(15, ' ')
  }


  val id     = Op("id")     { case t                   ⇒ t                   }
  val introL = Op("introL") { case t                   ⇒ Node(Nil, t)        }
  val introR = Op("introR") { case t                   ⇒ Node(t, Nil)        }
  val unitL  = Op("unitL")  { case Node(Nil, r)        ⇒ r                   }
  val unitR  = Op("unitR")  { case Node(l, Nil)        ⇒ l                   }
  val swap   = Op("swap")   { case Node(l, r)          ⇒ Node(r, l)          }
  val assocL = Op("assocL") { case Node(l, Node(m, r)) ⇒ Node(Node(l, m), r) }
  val assocR = Op("assocR") { case Node(Node(l, m), r) ⇒ Node(l, Node(m, r)) }

  def mapL(op: Op): Op = Op(s"mapL(${op.name})") { case Node(l, r) if op(l).isDefined ⇒ Node(op(l).get, r) }
  def mapR(op: Op): Op = Op(s"mapR(${op.name})") { case Node(l, r) if op(r).isDefined ⇒ Node(l, op(r).get) }

  val shiftL  = mapR(swap) andThen assocL named "shiftL"
  val shiftR  = mapL(swap) andThen assocR named "shiftR"

  val isNil:   BinTree ⇒ Boolean = { case Nil ⇒ true;          case _ ⇒ false }
  val nilHead: BinTree ⇒ Boolean = { case Node(Nil, _) ⇒ true; case _ ⇒ false }
  val nilInit: BinTree ⇒ Boolean = { case Node(_, Nil) ⇒ true; case _ ⇒ false }

  val reverseR: Op = nonNil(foldR(shiftR)) named "reverseR"
  val reverseL: Op = nonNil(foldL(shiftL)) named "reverseL"

  def foldR(op: Op) = bracketR(op.repeatUntil(nilHead))
  def foldL(op: Op) = bracketL(op.repeatUntil(nilInit))

  def bracketR(op: Op) = introR andThen op andThen unitL
  def bracketL(op: Op) = introL andThen op andThen unitR

  def nonNil(op: Op) = cond(isNil, id, op)

  def cond(p: BinTree ⇒ Boolean, ifTrue: Op, ifFalse: Op): Op = new Op {
    def name: String = s"cond(p, ${ifTrue.name}, ${ifFalse.name}"

    val pf: PartialFunction[BinTree, BinTree] = {
      case tree if p(tree) && ifTrue.pf.isDefinedAt(tree) ⇒ ifTrue(tree).get
      case tree if ifFalse.pf.isDefinedAt(tree) ⇒ ifFalse(tree).get
    }
  }

  def findRun(from: BinTree, to: BinTree): Unit = {
    @tailrec def run(current: BinTree, ops: List[Op]): Option[BinTree] = ops match {
      case scala.Nil ⇒ Some(current)
      case _ ⇒ ops.head(current) match {
        case None ⇒ {
          println(s"${ops.head} = None")
          None
        }
        case Some(next) ⇒ {
          println(s"${ops.head} = $next")

          run(next, ops.tail)
        }
      }
    }

    val steps = find(from, to).toList
    println(s"$from    →     $to")
    println("=====================================================================")
    run(from, steps)
    println("=====================================================================")
  }

  def find(from: BinTree, to: BinTree, max: Int = 20): Stream[Op] = {
    @tailrec def attempt(current: BinTree, todo: Stream[Op], done: Stream[Op], count: Int): Option[Stream[Op]] = {
      if (current == to) Some(done) else if (count > max) None else {
        todo.head(current) match {
          case None ⇒ None
          case Some(next) ⇒ if (next == current) {
            attempt(next, todo.tail, done, count)
          } else {
            attempt(next, todo.tail, done :+ todo.head, count + 1)
          }
        }
      }
    }

    Stream.continually(attempt(from, randOps, Stream.empty, 0)).flatten.head
  }

  def randOps: Stream[Op] = Stream.continually(rand)

  def rand: Op = {
    random.nextInt(3) match {
      case 0 ⇒ randSimpleOp
      case 1 ⇒ mapL(randSimpleOp)
      case 2 ⇒ mapR(randSimpleOp)
    }
  }

  def randSimpleOp: Op = ops(random.nextInt(ops.size - 1))

  val random = new Random()

  val ops = List(introL, introR, unitL, unitR, swap, assocL, assocR, shiftL, shiftR)
}

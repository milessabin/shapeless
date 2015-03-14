package shapeless

import scala.annotation.tailrec
import scala.util.Random

class BinTreeTests {
  import org.junit.Assert._
  import org.junit.Test

  @Test def testIsNil(): Unit = {
    assertTrue(isNil(Nil))
    assertFalse(isNil(a))
    assertFalse(isNil(Node(a, Nil)))
  }

  @Test def testNilInit(): Unit = {
    assertTrue(nilInit(Node(Nil, a)))
    assertFalse(nilInit(Nil))
    assertFalse(nilInit(Node(a, Nil)))
  }

  @Test def testNilTail(): Unit = {
    assertTrue(nilTail(Node(a, Nil)))
    assertFalse(nilTail(Nil))
    assertFalse(nilTail(Node(Nil, a)))
  }

  @Test def testId(): Unit = {
    assertEquals(Some(Nil), id(Nil))
    assertEquals(Some(a), id(a))
    assertEquals(Some(Node(a, Nil)), id(Node(a, Nil)))
  }

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

  @Test def testAssocL(): Unit = {
    assertEquals(Some(Node(Node(a, b), c)), assocL(Node(a, Node(b, c))))
    assertEquals(None, assocL(a))
    assertEquals(None, assocL(Node(Node(a, b), c)))
  }

  @Test def testAssocR(): Unit = {
    assertEquals(Some(Node(a, Node(b, c))), assocR(Node(Node(a, b), c)))
    assertEquals(None, assocR(a))
    assertEquals(None, assocR(Node(a, Node(b, c))))
  }

  @Test def testMapL(): Unit = {
    assertEquals(Some(Node(Node(b, a), c)), mapL(swap)(Node(Node(a, b), c)))
  }

  @Test def testMapR(): Unit = {
    assertEquals(Some(Node(a, Node(c, b))), mapR(swap)(Node(a, Node(b, c))))
  }

  @Test def testShiftL(): Unit = {
    assertEquals(Some(Node(Node(a, c), b)), shiftL(Node(a, Node(b, c))))
    assertEquals(None, shiftL(a))
    assertEquals(None, shiftL(Node(Node(a, b), c)))
  }

  @Test def testShiftR(): Unit = {
    assertEquals(Some(Node(b, Node(a, c))), shiftR(Node(Node(a, b), c)))
    assertEquals(None, shiftR(a))
    assertEquals(None, shiftR(Node(a, Node(b, c))))
  }

  @Test def testNonNil(): Unit = {
    assertEquals(Some(Nil), nonNil(swap)(Nil))
    assertEquals(Some(Node(b, a)), nonNil(swap)(Node(a, b)))
  }

  @Test def testBracketR(): Unit = {
    assertEquals(Some(Nil), bracketR(swap)(Nil))
    assertEquals(Some(a), bracketR(swap)(a))
    assertEquals(Some(Node(a, Nil)), bracketR(swap)(Node(a, Nil)))
  }

  @Test def testBracketL(): Unit = {
    assertEquals(Some(Nil), bracketL(swap)(Nil))
    assertEquals(Some(a), bracketL(swap)(a))
    assertEquals(Some(Node(a, Nil)), bracketL(swap)(Node(a, Nil)))
  }

  @Test def testRepeatUntil(): Unit = {
    assertEquals(Some(Node(a, Nil)), swap.repeatUntil(nilTail)(Node(a, Nil)))
    assertEquals(Some(Node(a, Nil)), swap.repeatUntil(nilTail)(Node(Nil, a)))
  }

  @Test def testFoldL(): Unit = {
    assertEquals(Some(Nil), foldL(swap)(Nil))
    assertEquals(Some(Node(a, Nil)), foldL(swap)(Node(a, Nil)))
  }

  @Test def testFoldR(): Unit = {
    assertEquals(Some(Nil), foldR(swap)(Nil))
    assertEquals(Some(Node(a, Nil)), foldR(swap)(Node(a, Nil)))
  }

  @Test def testReverseL(): Unit = {
    assertEquals(Some(                         Nil), reverseL(Nil))
    assertEquals(Some(                    Nil :# a), reverseL(Nil :# a))
    assertEquals(Some(               Nil :# b :# a), reverseL(Nil :# a :# b))
    assertEquals(Some(          Nil :# c :# b :# a), reverseL(Nil :# a :# b :# c))
    assertEquals(Some(     Nil :# d :# c :# b :# a), reverseL(Nil :# a :# b :# c :# d))
    assertEquals(Some(Nil :# e :# d :# c :# b :# a), reverseL(Nil :# a :# b :# c :# d :# e))
  }

  @Test def testReverseR(): Unit = {
    assertEquals(Some(                         Nil), reverseR(Nil))
    assertEquals(Some(                    a #: Nil), reverseR(a #: Nil))
    assertEquals(Some(               b #: a #: Nil), reverseR(a #: b #: Nil))
    assertEquals(Some(          c #: b #: a #: Nil), reverseR(a #: b #: c #: Nil))
    assertEquals(Some(     d #: c #: b #: a #: Nil), reverseR(a #: b #: c #: d #: Nil))
    assertEquals(Some(e #: d #: c #: b #: a #: Nil), reverseR(a #: b #: c #: d #: e #: Nil))
  }

  @Test def testFullL(): Unit = {
    assertEquals(Some(Nil), fullL(Nil))
    assertEquals(Some(Nil #: a), fullL(a :# Nil))
    assertEquals(Some(Nil #: a #: b), fullL(a :# b :# Nil))
  }

  @Test def testFullR(): Unit = {
    assertEquals(Some(Nil), fullR(Nil))
    assertEquals(Some(a :# Nil), fullR(Nil #: a))
    assertEquals(Some(a :# b :# Nil), fullR(Nil #: a #: b))
  }

  def testInitLastL(): Unit = {
    assertEquals(None, initLastL(Nil))
    assertEquals(Some(Nil #: a), initLastL(Nil #: a))
    assertEquals(Some(Nil #: a #: b), initLastL(Nil #: a #: b))
  }

  def testInitLastR(): Unit = {
    for {
      s1 ← id trace listL(a, b, c, d, e)
//      s2 ← fullR trace s1
      s2 ← headTailL trace s1
    } yield s2

    assertEquals(None, initLastR(listL()))
    assertEquals(Some(Nil #: a), initLastR(a :# Nil))
    assertEquals(Some(Nil #: a #: b), initLastR(a :# b :# Nil))
  }

  def listR(nodes: BinTree*): BinTree = nodes.foldRight(Nil: BinTree)(_ #: _)
  def listL(nodes: BinTree*): BinTree = nodes.foldLeft(Nil: BinTree)(_ :# _)

  def demoReverse(): Unit = {
    println(reverseR)

    for {
      s1 ← introR trace a #: b #: c #: Nil
      s2 ← shiftR trace s1
      s3 ← shiftR trace s2
      s4 ← shiftR trace s3
      s3 ← unitL  trace s4
    } yield ()

    println(reverseL)

    for {
      s1 ← introL trace Nil :# a :# b :# c
      s2 ← shiftL trace s1
      s3 ← shiftL trace s2
      s4 ← shiftL trace s3
      s5 ← unitR  trace s4
    } yield ()
  }

  def findReverse(): Unit = {
    findRun(a #: b #: Nil,                               b #: a #: Nil)
    findRun(a #: b #: c #: Nil,                     c #: b #: a #: Nil)
    findRun(a #: b #: c #: d #: Nil,           d #: c #: b #: a #: Nil)
    findRun(a #: b #: c #: d #: e #: Nil, e #: d #: c #: b #: a #: Nil)
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
    def #:(right: BinTree): BinTree = Node(right, this) // cons
    def :#(left: BinTree):  BinTree = Node(this, left)  // snoc

    def leaves: Stream[Char]
  }

  case object Nil extends BinTree {
    override def toString: String = "0"

    def leaves: Stream[Char] = Stream.empty
  }

  case class Leaf(value: Char) extends BinTree {
    override def toString: String = value.toString

    def leaves: Stream[Char] = Stream(value)
  }

  case class Node(left: BinTree, right: BinTree) extends BinTree {
    override def toString: String = s"($left, $right)"

    def leaves: Stream[Char] = left.leaves ++ right.leaves
  }


  case class BinTreePredicate(name: String)(pf: PartialFunction[BinTree, Boolean]) extends (BinTree ⇒ Boolean) {
    def apply(tree: BinTree): Boolean = pf.lift(tree).getOrElse(false)
    override def toString(): String = name

    def cond(ifTrue: Op, ifFalse: Op): Op = new Op.Cond(this, ifTrue, ifFalse)
  }


  object Op {
    def apply(name: String)(pf: PartialFunction[BinTree, BinTree]): Op = create(name)(pf.lift)

    def create(name0: String)(f: BinTree ⇒ Option[BinTree]): Op = new Op {
      def name = name0
      def apply(tree: BinTree): Option[BinTree] = f(tree)
    }


    class AndThen(before: Op, after: Op) extends Op {
      val name = s"${before.name} >>> ${after.name}"
      def apply(tree: BinTree): Option[BinTree] = before(tree).flatMap(after(_))
    }

    class RepeatUntil(op: Op, p: BinTreePredicate) extends Op {
      val name: String = s"repeat(${op.name}).until($p)"

      @tailrec final def apply(tree: BinTree): Option[BinTree] = {
        if (p(tree)) Some(tree) else op(tree) match {
          case None ⇒ None
          case Some(next) ⇒ apply(next)
        }
      }
    }

    class Named(op: Op, val name: String) extends Op {
      def apply(tree: BinTree): Option[BinTree] = op(tree)
      override def toString: String = s"$name = (${op.name})"
    }

    class Cond(p: BinTreePredicate, ifTrue: Op, ifFalse: Op) extends Op {
      def name: String = s"if ($name) { ${ifTrue.name} } else { ${ifFalse.name} }"
      def apply(tree: BinTree): Option[BinTree] = if (p(tree)) ifTrue(tree) else ifFalse(tree)
    }
  }

  trait Op {
    def name: String

    def trace(tree: BinTree): Option[BinTree] = {
      val result = apply(tree)

      println(s"$name ($tree) = $result")

      result
    }

    def apply(tree: BinTree): Option[BinTree]
    def andThen(after: Op): Op = new Op.AndThen(this, after)
    def repeatUntil(p: BinTreePredicate): Op = new Op.RepeatUntil(this, p)
    def named(name: String) = new Op.Named(this, name)

    override def toString: String = name.padTo(15, ' ')
  }

  val isNil   = BinTreePredicate("isNil")   { case Nil ⇒ true          }
  val nilInit = BinTreePredicate("nilInit") { case Node(Nil, _) ⇒ true }
  val nilTail = BinTreePredicate("nilTail") { case Node(_, Nil) ⇒ true }

  val id     = Op("id")     { case t                   ⇒ t                   }
  val introL = Op("introL") { case t                   ⇒ Node(Nil, t)        }
  val introR = Op("introR") { case t                   ⇒ Node(t, Nil)        }
  val unitL  = Op("unitL")  { case Node(Nil, r)        ⇒ r                   }
  val unitR  = Op("unitR")  { case Node(l, Nil)        ⇒ l                   }
  val swap   = Op("swap")   { case Node(l, r)          ⇒ Node(r, l)          }
  val assocL = Op("assocL") { case Node(l, Node(m, r)) ⇒ Node(Node(l, m), r) }
  val assocR = Op("assocR") { case Node(Node(l, m), r) ⇒ Node(l, Node(m, r)) }

  def mapL(op: Op): Op = Op.create(s"mapL(${op.name})") {
    case Node(l, r) ⇒ op(l).map(Node(_, r))
    case _ ⇒ None
  }

  def mapR(op: Op): Op = Op.create(s"mapR(${op.name})") {
    case Node(l, r) ⇒ op(r).map(Node(l, _))
    case _ ⇒ None
  }

  val shiftL  = mapR(swap) andThen assocL named "shiftL"
  val shiftR  = mapL(swap) andThen assocR named "shiftR"

  // TODO add orElse
  def nonNil(op: Op) = isNil.cond(id, op)

  def bracketR(op: Op) = introR andThen op andThen unitL
  def bracketL(op: Op) = introL andThen op andThen unitR

  def foldL(op: Op) = nonNil(bracketL(op.repeatUntil(nilTail)))
  def foldR(op: Op) = nonNil(bracketR(op.repeatUntil(nilInit)))

  val reverseL: Op = foldL(shiftL) named "reverseL"
  val reverseR: Op = foldR(shiftR) named "reverseR"

  val fullL: Op = foldL(assocL) named "fullL"
  val fullR: Op = foldR(assocR) named "fullR"

  val initLastL: Op = id named "initLastL"
  val initLastR: Op = nonNil(fullL andThen mapL(fullR)) named "initLastR"

  val headTailL: Op = nonNil(fullR andThen mapR(fullL)) named "headTailR"
  val headTailR: Op = id named "headTailR"


  def findRun(from: BinTree, to: BinTree, initial: Op = id): Unit = {
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

    val steps = find(from, to, initial).toList
    println(s"$from    →     $to")
    println("=====================================================================")
    run(from, steps)
    println("=====================================================================")
  }

  def find(from: BinTree, to: BinTree, initial: Op, max: Int = 20): Stream[Op] = {
    require(from.leaves.toSet == to.leaves.toSet, "Cannot create nor destroy information")

    @tailrec def attempt(current: BinTree, todo: Stream[Op], seen: Set[BinTree], done: Stream[Op], count: Int): Option[Stream[Op]] = {
      if (current == to) Some(done) else if (count > max || seen.contains(current) || current.leaves.count(_ == '0') > 2) None else {
        todo.head(current) match {
          case None ⇒ None
          case Some(next) ⇒ if (next == current) {
            attempt(next, todo.tail, seen + current, done, count)
          } else {
            attempt(next, todo.tail, seen + current, done :+ todo.head, count + 1)
          }
        }
      }
    }

    Stream.continually({
      val ops = initial #:: randOps
      // println("Attempting: " + ops.take(max).toList)
//      Thread.sleep(1000)
      attempt(from, ops, Set(), Stream.empty, 0)
    }).flatten.head
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

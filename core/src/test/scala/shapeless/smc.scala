package shapeless

import shapeless.OpticDefns.compose._
import shapeless.Typeable.ValueTypeable


class SMCTests {
  import org.junit.Test
  import shapeless.testutil._

  import SMCOps._
  import nat._3

  implicitly[RightLeaning[(Boolean, Unit)]]
  implicitly[RightLeaning[(Int, (Boolean, Unit))]]
  implicitly[RightLeaning[(String, (Int, (Boolean, Unit)))]]
  implicitly[RightLeaning[Either[Boolean, Unit]]]
  implicitly[RightLeaning[Either[Int, Either[Boolean, Unit]]]]
  implicitly[RightLeaning[Either[String, Either[Int, Either[Boolean, Unit]]]]]


  @Test def testHeadAndTail(): Unit = {
    val p: (String, (Int, (Boolean, Unit))) = ("foo", (23, (true, ())))

    val ph = p.head
    assertTypedEquals[String]("foo", ph)

    val pt = p.tail
    assertTypedEquals[(Int, (Boolean, Unit))]((23, (true, ())), pt)

    val c: Either[String, Either[Int, Either[Boolean, Unit]]] = Left("foo")

    val ch = c.head
    assertTypedEquals[Option[String]](Some("foo"), ch)

    val ct = c.tail
    assertTypedEquals[Option[Either[Int, Either[Boolean, Unit]]]](None, ct)
  }

  @Test def testLength(): Unit = {
    val pl = ("foo", (23, (true, ()))).length
    assertTypedEquals[_3](_3, pl)

    val cl = (Left("foo"): Either[String, Either[Int, Either[Boolean, Unit]]]).length
    assertTypedEquals[_3](_3, cl)
  }

  @Test def testReverse(): Unit = {
    val prev = ("foo", (23, (true, ()))).reverse
    assertTypedEquals[(Boolean, (Int, (String, Unit)))]((true, (23, ("foo", ()))), prev)

    val crev = (Left("foo"): Either[String, Either[Int, Either[Boolean, Unit]]]).reverse
    assertTypedEquals[Either[Boolean, Either[Int, Either[String, Unit]]]](Right(Right(Left("foo"))), crev)
  }

  @Test def testMap(): Unit = {
    val popt = ("foo", (23, (true, ()))).map(option)
    assertTypedEquals[(Option[String], (Option[Int], (Option[Boolean], Unit)))](
      (Some("foo"), (Some(23), (Some(true), ()))), popt
    )

    val copt = (Left("foo"): Either[String, Either[Int, Either[Boolean, Unit]]]).map(option)
    assertTypedEquals[Either[Option[String], Either[Option[Int], Either[Option[Boolean], Unit]]]](
      Left(Some("foo")), copt
    )
  }

  @Test def testRightAssoc(): Unit = {
    val p = ((("foo", 23), true), ())
    val pRA = p.rightAssoc
    assertTypedEquals[(String, (Int, (Boolean, Unit)))](("foo", (23, (true, ()))), pRA)

    val piRA = (2, ()).rightAssoc
    assertTypedEquals[(Int, Unit)]((2, ()), piRA)

    val priiRA = ((2, 3), ()).rightAssoc
    assertTypedEquals[(Int, (Int, Unit))]((2, (3, ())), priiRA)

    val ppRA = (p, p).rightAssoc
    assertTypedEquals[(String, (Int, (Boolean, (Unit, (String, (Int, (Boolean, Unit)))))))](
      ("foo", (23, (true, ((), ("foo", (23, (true, (())))))))),
      ppRA
    )
  }

  @Test def testLeftAssoc(): Unit = {
    val p = ("foo", (23, (true, ())))
    val pLA = p.leftAssoc
    assertTypedEquals[(((String, Int), Boolean), Unit)](((("foo", 23), true), ()), pLA)

    val piLA = (2, ()).leftAssoc
    assertTypedEquals[(Int, Unit)]((2, ()), piLA)

    val priiLA = (2, (3, ())).leftAssoc
    assertTypedEquals[((Int, Int), Unit)](((2, 3), ()), priiLA)

    val ppLA = (p, p).leftAssoc
    assertTypedEquals[(((((((String, Int), Boolean), Unit), String), Int), Boolean), Unit)](
      ((((((("foo", 23), true), ()), "foo"), 23), true), ()),
      ppLA
    )
  }

  trait PolyFoldLeft extends Poly2 {
    // Does it make sense to unify 'FoldLeftPBuilder' & 'FoldLeftCBuilder' (by using SMC), or would that be cyclic ?
    class FoldLeftPBuilder[Elem, Acc, A, B] {
      def apply[R](fn: (Elem, Acc) => R) = at[((Elem, A), B), Acc]((a: ((Elem, A), B), acc: Acc) => fn(a._1._1, acc))
    }

    class FoldLeftCBuilder[Elem, Acc, A, B] {
      def apply(fn: (Elem, Acc) => Acc) =
        at[Either[Either[Elem, A], B], Acc].apply[Acc]((e, acc) => {
          e.left.toOption.flatMap(_.left.toOption) match {
            case Some(elem) => fn(elem, acc)
            case None => acc
          }
        })
    }

    class FoldLeftDoneBuilder[A, Acc] {
      def apply[Res](f: Acc => Res) = at[A, Acc]((a: A, acc: Acc) => f(acc))
    }

    def foldLeftP[Elem, Acc, A, B] = new FoldLeftPBuilder[Elem, Acc, A, B]
    def foldLeftC[Elem, Acc, A, B] = new FoldLeftCBuilder[Elem, Acc, A, B]
    def foldLeftDone[A, Acc]       = new FoldLeftDoneBuilder[A, Acc]
  }

  private object lengthP extends PolyFoldLeft {
    implicit def string[A, B] =
      foldLeftP[String, Int, A, B]((s: String, acc: Int) => s.length + acc)

    implicit def int[A, B] =
      foldLeftP[Int, Int, A, B]((i: Int, acc: Int) => i + acc)

    implicit def boolean[A, B] =
      foldLeftP[Boolean, Int, A, B]((b: Boolean, acc: Int) => (if (b) 2 else 1) + acc)

    implicit def done[A] =
      foldLeftDone[A, Int](acc => acc)
  }

  private object lengthC extends PolyFoldLeft {
    implicit def string[A, B]  =
      foldLeftC[String, Int, A, B]((s: String, acc: Int) => s.length + acc)

    implicit def int[A, B]     =
      foldLeftC[Int, Int, A, B]((i: Int, acc: Int) => i + acc)

    implicit def boolean[A, B] =
      foldLeftC[Boolean, Int, A, B]((b: Boolean, acc: Int) => (if (b) 2 else 1) + acc)

    implicit def done[A] =
      foldLeftDone[A, Int](acc => acc)
  }

  @Test def foldLeft(): Unit = {
    val p = ("foo", (23, (true, ())))
    val plength = p.foldLeft(0)(lengthP)
    assertTypedEquals[Int](28, plength)

    val c: Either[String, Either[Int, Either[Boolean, Unit]]] = Left("foo")
    val clength = c.foldLeft(0)(lengthC)
    assertTypedEquals[Int](3, clength)
  }
}

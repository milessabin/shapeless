package shapeless

import org.junit.Test

class LazyPriorityTests {

  case class CC1(i: Int)
  case class CC2(i: Int)

  sealed trait Tree0
  object Tree0 {
    case class Node(left: Tree0, right: Tree0, v: Int) extends Tree0
    case object Leaf extends Tree0
  }

  sealed trait Tree
  object Tree {
    case class Node(left: Tree, right: Tree, v: Int) extends Tree
    case object Leaf extends Tree

    // Not always found if put in Leaf
    implicit val tc: Definitions.TC[Leaf.type] = Definitions.TC.of[Leaf.type](_ => "Leaf")
  }

  object Definitions {
    trait TC[T] {
      def msg(n: Int): String
    }

    object TC {
      def apply[T](implicit tc: TC[T]): TC[T] = tc

      def of[T](msg0: Int => String): TC[T] =
        new TC[T] {
          def msg(n: Int) = if (n >= 0) msg0(n) else "…"
        }

      implicit val intTC: TC[Int] = of[Int](_ => "Int")
      implicit val booleanTC: TC[Boolean] = of[Boolean](_ => "Boolean")
      implicit def optionTC[T: TC]: TC[Option[T]] = of[Option[T]](n => s"Option[${TC[T].msg(n-1)}]")
      implicit def tuple2TC[A: TC, B: TC]: TC[(A, B)] = of[(A, B)](n => s"(${TC[A].msg(n-1)}, ${TC[B].msg(n-1)})")
      implicit val cc1: TC[CC1] = of[CC1](_ => "CC1")
    }
  }

  object Deriver {
    import Definitions._

    trait MkTC[T] {
      def tc: TC[T]
    }

    object MkTC {
      implicit def hnilMkTC: MkTC[HNil] =
        new MkTC[HNil] {
          val tc = TC.of[HNil](_ => "HNil")
        }
      implicit def hconsTC[H, T <: HList]
       (implicit
         head: Lazy[TC[H]],
         tail: Lazy[MkTC[T]]
       ): MkTC[H :: T] =
        new MkTC[H :: T] {
          lazy val tc = TC.of[H :: T](n => s"${head.value.msg(n-1)} :: ${tail.value.tc.msg(n-1)}")
        }
      implicit def cnilMkTC: MkTC[CNil] =
        new MkTC[CNil] {
          val tc = TC.of[CNil](_ => "CNil")
        }
      implicit def cconsTC[H, T <: Coproduct]
       (implicit
         head: Lazy[TC[H]],
         tail: Lazy[MkTC[T]]
       ): MkTC[H :+: T] =
        new MkTC[H :+: T] {
          lazy val tc = TC.of[H :+: T](n => s"${head.value.msg(n-1)} :+: ${tail.value.tc.msg(n-1)}")
        }
      implicit def genericTC[F, G]
       (implicit
         gen: Generic.Aux[F, G],
         underlying: Lazy[MkTC[G]]
       ): MkTC[F] =
        new MkTC[F] {
          lazy val tc = TC.of[F](n => s"Generic[${underlying.value.tc.msg(n-1)}]")
        }
    }

    implicit def mkTC[T]
     (implicit
       priority: Lazy[Priority[TC[T], MkTC[T]]]
     ): TC[T] =
      priority.value.fold(identity)(_.tc)
  }

  @Test
  def testLazyPriority(): Unit = {
    import Definitions._
    import Deriver._

    def validate[T: TC](expected: String, n: Int = Int.MaxValue): Unit = {
      val msg = TC[T].msg(n)
      assert(expected == msg)
    }

    // All orphans
    validate[Int]("Int")
    validate[CC1]("CC1")
    validate[Option[Int]]("Option[Int]")
    validate[Option[CC1]]("Option[CC1]")
    validate[(Int, CC1)]("(Int, CC1)")
    validate[(CC1, Int)]("(CC1, Int)")
    validate[(CC1, Boolean)]("(CC1, Boolean)")

    // Derived, then orphans
    validate[CC2]("Generic[Int :: HNil]")
    validate[Either[Int, CC1]]("Generic[Generic[Int :: HNil] :+: Generic[CC1 :: HNil] :+: CNil]")
    // Fails with the current Orphan
    validate[(Int, CC1, Boolean)]("Generic[Int :: CC1 :: Boolean :: HNil]")
    validate[(Int, CC2, Boolean)]("Generic[Int :: Generic[Int :: HNil] :: Boolean :: HNil]")

    // Orphan, then derived, then orphans
    validate[Option[CC2]]("Option[Generic[Int :: HNil]]")
    validate[(Int, CC2)]("(Int, Generic[Int :: HNil])")


    // Cycles

    // Derived (but for TC[Int])
    validate[Tree0.Leaf.type]("Generic[HNil]")
    validate[Tree0]("Generic[Generic[HNil] :+: Generic[Generic[Generic[HNil] :+: Generic[Generic[Generic[…] :+: … :+: …] :: Generic[… :+: …] :: Int :: HNil] :+: CNil] :: Generic[Generic[HNil] :+: Generic[Generic[… :+: …] :: Generic[…] :: … :: …] :+: CNil] :: Int :: HNil] :+: CNil]", 12)

    // Orphan
    validate[Tree.Leaf.type]("Leaf")
    // Interleaved derived / orphans
    // Fails with the current Orphan
    validate[Tree]("Generic[Leaf :+: Generic[Generic[Leaf :+: Generic[Generic[Leaf :+: … :+: …] :: Generic[… :+: …] :: Int :: HNil] :+: CNil] :: Generic[Leaf :+: Generic[Generic[… :+: …] :: Generic[…] :: … :: …] :+: CNil] :: Int :: HNil] :+: CNil]", 12)
  }

}


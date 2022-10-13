package shapeless

import org.junit.Test

object LowPriorityTests {

  object Simple {
    trait TC[T] {
      def prop: Boolean
    }

    object TC {
      def apply[T](implicit tc: TC[T]): TC[T] = tc

      implicit val intTC: TC[Int] = new TC[Int] { def prop = true }
    }

    case class CC(s: String)

    object CC {
      implicit val ccTC: TC[CC] = new TC[CC] { def prop = true }
    }

    case class CC2(s: String)

    object Extra {
      implicit def extraTC[T](implicit ev: LowPriority): TC[T] =
        new TC[T] { def prop = false }
    }
  }

  object WithIgnoring {
    trait TC[T] {
      def prop: Option[Boolean]
    }

    trait LowPriTC {
      implicit def anyTC[T]: TC[T] = new TC[T] { def prop = None }
    }

    object TC extends LowPriTC {
      def apply[T](implicit tc: TC[T]): TC[T] = tc

      implicit val intTC: TC[Int] = new TC[Int] { def prop = Some(true) }
    }

    case class CC(s: String)

    object CC {
      implicit val ccTC: TC[CC] = new TC[CC] { def prop = Some(true) }
    }

    case class CC2(s: String)

    object Extra {
      implicit def extraTC[T](implicit ev: LowPriority.Ignoring[Witness.`"anyTC"`.T]): TC[T] =
        new TC[T] { def prop = Some(false) }
    }
  }

}

class LowPriorityTests {
  import LowPriorityTests._

  @Test
  def simple: Unit = {
    import Simple._
    import Extra._

    // `Extra` provides extra implicit instances of `TC[T]`
    // We check here that these do not take precedence over the already existing implicit instances.

    assert(TC[Int].prop)
    assert(TC[CC].prop)
    assert(!TC[String].prop)
    assert(!TC[CC2].prop)

    {
      implicit val cc2TC: TC[CC2] = new TC[CC2] { def prop = true }
      assert(TC[CC2].prop)
    }
  }

  @Test
  def withIgnoring: Unit = {
    import WithIgnoring._
    import Extra._

    // `Extra` provides extra implicit instances of `TC[T]`
    // We check here that these do not take precedence over the already existing implicit instances.

    assert(TC[Int].prop == Some(true))

    assert(TC[CC].prop == Some(true))
    assert(TC[String].prop == Some(false))
    assert(TC[CC2].prop == Some(false))

    {
      implicit val cc2TC: TC[CC2] = new TC[CC2] { def prop = Some(true) }
      assert(TC[CC2].prop == Some(true))
    }
  }

}

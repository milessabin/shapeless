package shapeless

import org.junit.Test

object CachedTestsDefinitions {

  trait Intermediate {
    def repr: String
  }

  object Intermediate {
    def apply()(implicit intm: Intermediate): Intermediate = intm

    implicit val default: Intermediate =
      new Intermediate {
        val repr = "default"
      }
  }

  object ScalaDocExample {
    trait TC[T] {
      def msg: String
    }

    object First {
      implicit val tc: TC[Int] = new TC[Int] {
        val msg = "first"
      }

      def msg = implicitly[TC[Int]].msg
      def msgCached = Cached.implicitly[TC[Int]].msg
    }

    object Second {
      implicit val tc: TC[Int] = new TC[Int] {
        val msg = "second"
      }

      def msg = implicitly[TC[Int]].msg
      def msgCached = Cached.implicitly[TC[Int]].msg
    }
  }

}

class CachedTests {
  import CachedTestsDefinitions._

  @Test
  def simple {
    val first = Intermediate()
    val cachedFirst = Cached.implicitly[Intermediate]
    assert(first.repr == "default")
    assert(cachedFirst.repr == "default")

    {
      implicit val overrideIntm: Intermediate =
        new Intermediate {
          val repr = "override"
        }

      val second = Intermediate()
      val cachedSecond = Cached.implicitly[Intermediate]
      assert(second.repr == "override")
      assert(cachedSecond.repr == "default")
    }
  }

  @Test
  def scalaDocExample {
    import ScalaDocExample._
    
    val first = First.msg
    val second = Second.msg
    val firstCached = First.msgCached
    val secondCached = Second.msgCached

    assert(first == "first")
    assert(second == "second")
    assert(firstCached == "first")
    assert(secondCached == "first")
  }

}

package shapeless

import shapeless.record.Record

import org.junit.Test
import org.junit.Assert._
import shapeless.test.illTyped
import shapeless.testutil.assertTypedEquals
import labelled.->>

// Intentionally defined as a top-level class - (compile time) reflection API not behaving
// the same way compared to definitions in a singleton, like CC below.
// See https://github.com/milessabin/shapeless/issues/474
case class DefaultCC(i: Int, s: String = "b", flagOpt: Option[Boolean] = Some(true))

object DefaultTestDefinitions {

  case class CC(i: Int, s: String = "b", flagOpt: Option[Boolean] = Some(true))

  sealed trait Base
  case class BaseI(i: Int) extends Base

  trait Dummy

  trait Definitions {
    case class CC(i: Int, s: String = "b", flagOpt: Option[Boolean] = Some(true))
  }

  val definitions = new Definitions {}

  object ApplyWithDefault1 {
    case class CC(i: Int, s: String)
    object CC {
      def apply(i: Int = 0): CC = CC(i, "")
    }
  }

  object ApplyWithDefault2 {
    case class CC(i: Int, s: String)
    object CC {
      def apply(i: Int, d: Int = 0): CC = CC(i, d.toString)
    }
  }

  object SemiAuto {
    case class CCl1(i: Int = 0)
    object CCl1

    case class CCl2(i: Int)
    trait CCl2Companion
    object CCl2 extends CCl2Companion

    case object CObj
  }

  class DefaultRun extends Exception("Default value was run")
  case class SideEffectingDefault(n: Int = throw new DefaultRun)
}

class DefaultTests {
  import DefaultTestDefinitions._

  @Test
  def simple: Unit = {
    val default = Default[CC].apply()
    assertTypedEquals[None.type :: Some[String] :: Some[Option[Boolean]] :: HNil](
      None :: Some("b") :: Some(Some(true)) :: HNil,
      default
    )
  }

  @Test
  def topLevel: Unit = {
    // See https://github.com/milessabin/shapeless/issues/474
    val default = Default[DefaultCC].apply()
    assertTypedEquals[None.type :: Some[String] :: Some[Option[Boolean]] :: HNil](
      None :: Some("b") :: Some(Some(true)) :: HNil,
      default
    )
  }

  @Test
  def simpleFromPath: Unit = {
    val default = Default[definitions.CC].apply()
    assertTypedEquals[None.type :: Some[String] :: Some[Option[Boolean]] :: HNil](
      None :: Some("b") :: Some(Some(true)) :: HNil,
      default
    )
  }

  @Test
  def invalid: Unit = {
    illTyped(" Default[Base] ")

    illTyped(" Default[Dummy] ")

    illTyped(" Default[Any] ")
    illTyped(" Default[AnyRef] ")
    illTyped(" Default[Array[Int]] ")
  }

  @Test
  def simpleAsRecord: Unit = {
    val default = Default.AsRecord[CC].apply()
    assertTypedEquals[("s" ->> String) :: ("flagOpt" ->> Option[Boolean]) :: HNil](
      Record(s = "b", flagOpt = Some(true)),
      default
    )
  }

  @Test
  def simpleFromPathAsRecord: Unit = {
    val default = Default.AsRecord[definitions.CC].apply()
    assertTypedEquals[("s" ->> String) :: ("flagOpt" ->> Option[Boolean]) :: HNil](
      Record(s = "b", flagOpt = Some(true)),
      default
    )
  }

  @Test
  def invalidAsRecord: Unit = {
    illTyped(" Default.AsRecord[Base] ")

    illTyped(" Default.AsRecord[Dummy] ")

    illTyped(" Default.AsRecord[Any] ")
    illTyped(" Default.AsRecord[AnyRef] ")
    illTyped(" Default.AsRecord[Array[Int]] ")
  }

  @Test
  def simpleAsOptions: Unit = {
    illTyped(
      " val default0: None.type :: Some[String] :: Some[Option[Boolean]] :: HNil = Default.AsOptions[CC].apply() ",
      "type mismatch.*"
    )

    val default = Default.AsOptions[CC].apply()
    assertTypedEquals[Option[Int] :: Option[String] :: Option[Option[Boolean]] :: HNil](
      None :: Some("b") :: Some(Some(true)) :: HNil,
      default
    )
  }

  @Test
  def simpleFromPathAsOptions: Unit = {
    illTyped(
      " val default0: None.type :: Some[String] :: Some[Option[Boolean]] :: HNil = Default.AsOptions[definitions.CC].apply() ",
      "type mismatch.*"
    )

    val default = Default.AsOptions[definitions.CC].apply()
    assertTypedEquals[Option[Int] :: Option[String] :: Option[Option[Boolean]] :: HNil](
      None :: Some("b") :: Some(Some(true)) :: HNil,
      default
    )
  }

  @Test
  def invalidAsOptions: Unit = {
    illTyped(" Default.AsOptions[Base] ")

    illTyped(" Default.AsOptions[Dummy] ")

    illTyped(" Default.AsOptions[Any] ")
    illTyped(" Default.AsOptions[AnyRef] ")
    illTyped(" Default.AsOptions[Array[Int]] ")
  }

  @Test
  def localClass: Unit = {
    case class Default0(d: Double = 1.0)

    val default0 = Default[Default0].apply()
    assertTypedEquals[Some[Double] :: HNil](
      Some(1.0) :: HNil,
      default0
    )

    case class Default1(d: Double, s: String = "b", df: Default0 = Default0())

    val default1 = Default[Default1].apply()
    assertTypedEquals[None.type :: Some[String] :: Some[Default0] :: HNil](
      None :: Some("b") :: Some(Default0()) :: HNil,
      default1
    )
  }

  @Test
  def applyWithDefault1: Unit = {
    val default = Default[ApplyWithDefault1.CC].apply()
    assertTypedEquals[None.type :: None.type :: HNil](
      None :: None :: HNil,
      default
    )
  }

  @Test
  def applyWithDefault2: Unit = {
    val default = Default[ApplyWithDefault2.CC].apply()
    assertTypedEquals[None.type :: None.type :: HNil](
      None :: None :: HNil,
      default
    )
  }

  @Test
  def testSemiAuto: Unit = {
    import SemiAuto._

    val default1 = Default[CCl1]
    val default2 = Default[CCl2]
    val default3 = Default[CObj.type]

    assertTypedEquals[Some[Int] :: HNil](Some(0) :: HNil, default1())
    assertTypedEquals[None.type :: HNil](None :: HNil, default2())
    assertTypedEquals[HNil](HNil, default3())
  }

  @Test
  def testByName: Unit = {
    val default = Default[SideEffectingDefault]
    val thrownException = try {
      default()
      false
    } catch {
      case _: DefaultRun =>
        true
    }
    assert(thrownException, "Expected DefaultRun to be thrown")
  }
}

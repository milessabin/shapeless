package shapeless

import shapeless.record.Record

import org.junit.Test
import shapeless.test.illTyped
import shapeless.testutil.assertTypedEquals

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

}

class DefaultTests {
  import DefaultTestDefinitions._

  @Test
  def simple {
    val default = Default[CC].apply()
    assertTypedEquals[None.type :: Some[String] :: Some[Option[Boolean]] :: HNil](
      None :: Some("b") :: Some(Some(true)) :: HNil,
      default
    )
  }

  @Test
  def topLevel {
    // See https://github.com/milessabin/shapeless/issues/474
    val default = Default[DefaultCC].apply()
    assertTypedEquals[None.type :: Some[String] :: Some[Option[Boolean]] :: HNil](
      None :: Some("b") :: Some(Some(true)) :: HNil,
      default
    )
  }

  @Test
  def simpleFromPath {
    val default = Default[definitions.CC].apply()
    assertTypedEquals[None.type :: Some[String] :: Some[Option[Boolean]] :: HNil](
      None :: Some("b") :: Some(Some(true)) :: HNil,
      default
    )
  }

  @Test
  def invalid {
    illTyped(" Default[Base] ", "could not find implicit value for parameter default: .*")

    illTyped(" Default[Dummy] ", "could not find implicit value for parameter default: .*")

    illTyped(" Default[Any] ", "could not find implicit value for parameter default: .*")
    Default[AnyRef] // this one shouldn't compile - related to https://github.com/milessabin/shapeless/issues/453
    illTyped(" Default[Array[Int]] ", "could not find implicit value for parameter default: .*")
  }

  @Test
  def simpleAsRecord {
    val default = Default.AsRecord[CC].apply()
    assertTypedEquals[Record.`'s -> String, 'flagOpt -> Option[Boolean]`.T](
      Record(s = "b", flagOpt = Some(true)),
      default
    )
  }

  @Test
  def simpleFromPathAsRecord {
    val default = Default.AsRecord[definitions.CC].apply()
    assertTypedEquals[Record.`'s -> String, 'flagOpt -> Option[Boolean]`.T](
      Record(s = "b", flagOpt = Some(true)),
      default
    )
  }

  @Test
  def invalidAsRecord {
    illTyped(" Default.AsRecord[Base] ", "could not find implicit value for parameter default: .*")

    illTyped(" Default.AsRecord[Dummy] ", "could not find implicit value for parameter default: .*")

    illTyped(" Default.AsRecord[Any] ", "could not find implicit value for parameter default: .*")
    Default.AsRecord[AnyRef] // this one shouldn't compile - related to https://github.com/milessabin/shapeless/issues/453
    illTyped(" Default.AsRecord[Array[Int]] ", "could not find implicit value for parameter default: .*")
  }

  @Test
  def simpleAsOptions {
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
  def simpleFromPathAsOptions {
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
  def invalidAsOptions {
    illTyped(" Default.AsOptions[Base] ", "could not find implicit value for parameter default: .*")

    illTyped(" Default.AsOptions[Dummy] ", "could not find implicit value for parameter default: .*")

    illTyped(" Default.AsOptions[Any] ", "could not find implicit value for parameter default: .*")
    Default.AsOptions[AnyRef] // this one shouldn't compile - related to https://github.com/milessabin/shapeless/issues/453
    illTyped(" Default.AsOptions[Array[Int]] ", "could not find implicit value for parameter default: .*")
  }

  @Test
  def localClass {
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

}

package shapeless

import shapeless.record.Record

import org.junit.Test
import shapeless.test.illTyped

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
    {
      val default: None.type :: Some[String] :: Some[Option[Boolean]] :: HNil = Default[CC].apply()
      assert(default == None :: Some("b") :: Some(Some(true)) :: HNil)
    }

    {
      val default = Default[CC].apply()
      assert(default == None :: Some("b") :: Some(Some(true)) :: HNil)
    }
  }

  @Test
  def simpleFromPath {
    {
      val default: None.type :: Some[String] :: Some[Option[Boolean]] :: HNil = Default[definitions.CC].apply()
      assert(default == None :: Some("b") :: Some(Some(true)) :: HNil)
    }

    {
      val default = Default[definitions.CC].apply()
      assert(default == None :: Some("b") :: Some(Some(true)) :: HNil)
    }
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
    {
      val default: Record.`'s -> String, 'flagOpt -> Option[Boolean]`.T = Default.AsRecord[CC].apply()
      assert(default == Record(s = "b", flagOpt = Some(true)))
    }

    {
      val default = Default.AsRecord[CC].apply()
      assert(default == Record(s = "b", flagOpt = Some(true)))
    }
  }

  @Test
  def simpleFromPathAsRecord {
    {
      val default: Record.`'s -> String, 'flagOpt -> Option[Boolean]`.T = Default.AsRecord[definitions.CC].apply()
      assert(default == Record(s = "b", flagOpt = Some(true)))
    }

    {
      val default = Default.AsRecord[definitions.CC].apply()
      assert(default == Record(s = "b", flagOpt = Some(true)))
    }
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

    {
      val default: Option[Int] :: Option[String] :: Option[Option[Boolean]] :: HNil = Default.AsOptions[CC].apply()
      assert(default == None :: Some("b") :: Some(Some(true)) :: HNil)
    }

    {
      val default = Default.AsOptions[CC].apply()
      assert(default == None :: Some("b") :: Some(Some(true)) :: HNil)
    }
  }

  @Test
  def simpleFromPathAsOptions {
    illTyped(
      " val default0: None.type :: Some[String] :: Some[Option[Boolean]] :: HNil = Default.AsOptions[definitions.CC].apply() ",
      "type mismatch.*"
    )

    {
      val default: Option[Int] :: Option[String] :: Option[Option[Boolean]] :: HNil = Default.AsOptions[definitions.CC].apply()
      assert(default == None :: Some("b") :: Some(Some(true)) :: HNil)
    }

    {
      val default = Default[definitions.CC].apply()
      assert(default == None :: Some("b") :: Some(Some(true)) :: HNil)
    }
  }

  @Test
  def invalidAsOptions {
    illTyped(" Default.AsOptions[Base] ", "could not find implicit value for parameter default: .*")

    illTyped(" Default.AsOptions[Dummy] ", "could not find implicit value for parameter default: .*")

    illTyped(" Default.AsOptions[Any] ", "could not find implicit value for parameter default: .*")
    Default.AsOptions[AnyRef] // this one shouldn't compile - related to https://github.com/milessabin/shapeless/issues/453
    illTyped(" Default.AsOptions[Array[Int]] ", "could not find implicit value for parameter default: .*")
  }

}

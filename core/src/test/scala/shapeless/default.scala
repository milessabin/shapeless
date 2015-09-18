package shapeless

import org.junit.Test
import shapeless.test.illTyped

object DefaultTestDefinitions {

  case class CC(i: Int, s: String = "b", flagOpt: Option[Boolean] = Some(true))

  sealed trait Base
  case class BaseI(i: Int)

  trait Dummy

  trait Definitions {
    case class CC(i: Int, s: String = "b", flagOpt: Option[Boolean] = Some(true))
  }

  object DefinitionsObj extends Definitions

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
      val default: None.type :: Some[String] :: Some[Option[Boolean]] :: HNil = Default[DefinitionsObj.CC].apply()
      assert(default == None :: Some("b") :: Some(Some(true)) :: HNil)
    }

    {
      val default = Default[DefinitionsObj.CC].apply()
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

}

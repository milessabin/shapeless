package shapeless.test

import shapeless._
import syntax.typeable._

object TestTypeable {
  val foo: Any = null
  //foo.cast[Option[Int]]
  //foo.cast[List[Int]]

  //foo.cast[Option[Any]]
  //foo.cast[List[Any]]

  //foo.cast[Option[_]]
  //foo.cast[List[_]]

  //Typeable[Option[Any]]
  Typeable[Option[_]]

  Typeable[Either[Int, _]]
}


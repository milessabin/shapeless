package shapeless

import shapeless.test.illTyped

object GenericTests213 {
  case class WrongApplySignature private(value: String)
  object WrongApplySignature {
    // We can't replace the synthetic `apply` method on Scala 2.11
    def apply(v: String): Either[String, WrongApplySignature] = Left("No ways")
  }

  illTyped("Generic[WrongApplySignature]")
}

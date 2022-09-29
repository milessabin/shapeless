package shapeless.syntax

import shapeless.SingletonTypeMacros

import scala.language.experimental.macros

object singleton {
  implicit def mkSingletonOps(t: Any): SingletonOps =
    macro SingletonTypeMacros.mkSingletonOps
}

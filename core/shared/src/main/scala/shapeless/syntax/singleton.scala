package shapeless.syntax

import shapeless.Witness

object singleton {
  implicit def mkSingletonOps[T](t: T): SingletonOps.Aux[t.type] =
    SingletonOps.instance[t.type](Witness(t))
}

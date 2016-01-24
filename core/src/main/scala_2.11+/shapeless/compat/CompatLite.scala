package shapeless.compat

import scala.reflect.macros.whitebox

trait CompatLite {
  val c: whitebox.Context
  type ImplicitCandidate211 = c.ImplicitCandidate
}

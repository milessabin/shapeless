package shapeless

import Definitions._

import org.junit.Test


class Priority211Tests {

  @Test
  def simple {
    import SimpleTCDeriver._
    Lazy.mkLazy[TC[CC2]]
  }

}

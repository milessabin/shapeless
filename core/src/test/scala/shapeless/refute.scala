package shapeless

import org.junit.Test

import test._

object RefuteTests {
  class PresentEvidence
  implicit val presentEvidence: PresentEvidence = new PresentEvidence

  class NotPresentEvidence

  class InductiveEvidence
  implicit def inductiveEvidence(implicit ev: Refute[NotPresentEvidence]): InductiveEvidence = new InductiveEvidence
}

class RefuteTests {

  import RefuteTests._

  @Test
  def testRefuteNotFound {
    illTyped("the[Refute[PresentEvidence]]")
  }

  @Test
  def testRefuteNotPresent {
    the[Refute[NotPresentEvidence]]
  }

  @Test
  def testInductivePresent {
    the[InductiveEvidence]
  }

  @Test
  def increaseCodeCoverage {
    // Increases the code coverage, as the above tests cannot
    Refute.ambiguousIfPresent[PresentEvidence](presentEvidence)
    Refute.refute[PresentEvidence]
  }

}

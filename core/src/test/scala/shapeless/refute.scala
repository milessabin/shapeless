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
  def testRefuteNotFound: Unit = {
    illTyped("the[Refute[PresentEvidence]]")
  }

  @Test
  def testRefuteNotPresent: Unit = {
    the[Refute[NotPresentEvidence]]
  }

  @Test
  def testInductivePresent: Unit = {
    the[InductiveEvidence]
  }

  @Test
  def increaseCodeCoverage: Unit = {
    // Increases the code coverage, as the above tests cannot
    Refute.Aux.amb1[PresentEvidence](presentEvidence)
    Refute.Aux.amb2[PresentEvidence]
    Refute.refute(new Refute.Aux[PresentEvidence]{})
  }

}

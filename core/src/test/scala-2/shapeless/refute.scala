package shapeless

import org.junit.Test

import test._

class RefuteTestsScala2 {

  import RefuteTests._

  @Test
  def increaseCodeCoverage: Unit = {
    // Increases the code coverage, as the above tests cannot
    Refute.Impl.amb1[PresentEvidence](presentEvidence)
    Refute.Impl.amb2[PresentEvidence]
    Refute.refute(new Refute.Impl[PresentEvidence]{})
  }
}

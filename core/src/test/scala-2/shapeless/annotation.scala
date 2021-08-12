package shapeless

import scala.annotation.{ Annotation => saAnnotation }
import org.junit.Test
import shapeless.test.{illTyped, typed}

class AnnotationTestsScala2 {
  import AnnotationTestsDefinitions._

  /*
  @Test
  def allTypeAnnotations: Unit = {
    val st = AllTypeAnnotations[Base2].apply() // sealed trait
    typed[(First :: HNil) :: (Second :: Third :: HNil) :: HNil](st)
  }
  */
}

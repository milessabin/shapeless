/*
 * Copyright (c) 2015-19 Alexandre Archambault
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package shapeless

import scala.annotation.{ Annotation => saAnnotation }
import org.junit.Test
import shapeless.test.illTyped

object AnnotationTestsDefinitions {

  case class First() extends saAnnotation
  case class Second(i: Int, s: String) extends saAnnotation

  case class Other() extends saAnnotation
  case class Last(b: Boolean) extends saAnnotation

  case class Unused() extends saAnnotation

  @Other case class CC(
    @First i: Int,
    s: String,
    @Second(2, "b") ob: Option[Boolean]
  )

  @Last(true) trait Something

  sealed trait Base
  @First case class BaseI(i: Int) extends Base
  @Second(3, "e") case class BaseS(s: String) extends Base

  trait Abstract1
  abstract class Abstract2
}

class AnnotationTests {
  import AnnotationTestsDefinitions._

  @Test
  def simpleAnnotation: Unit = {
    {
      val other = Annotation[Other, CC].apply()
      assert(other == Other())

      val last = Annotation[Last, Something].apply()
      assert(last == Last(true))
    }

    {
      val other: Other = Annotation[Other, CC].apply()
      assert(other == Other())

      val last: Last = Annotation[Last, Something].apply()
      assert(last == Last(true))
    }
  }

  @Test
  def invalidAnnotation: Unit = {
    illTyped(" Annotation[Other, Abstract1] ", ".*no implicit argument.*")
    illTyped(" Annotation[Abstract1, CC] ", ".*no implicit argument.*")
    illTyped(" Annotation[Abstract2, CC] ", ".*no implicit argument.*")
  }

  @Test
  def simpleAnnotations: Unit = {
    {
      val first: (Some[First], None.type, None.type) = Annotations[First, CC].apply()
      assert(first == (Some(First()), None, None))

      val second: (None.type, None.type, Some[Second]) = Annotations[Second, CC].apply()
      assert(second == (None, None, Some(Second(2, "b"))))

      val unused: (None.type, None.type, None.type) = Annotations[Unused, CC].apply()
      assert(unused == (None, None, None))

      val firstSum: (Some[First], None.type) = Annotations[First, Base].apply()
      assert(firstSum == (Some(First()), None))

      val secondSum: (None.type, Some[Second]) = Annotations[Second, Base].apply()
      assert(secondSum == (None, Some(Second(3, "e"))))
    }

    {
      val first = Annotations[First, CC].apply()
      assert(first == (Some(First()), None, None))

      val second = Annotations[Second, CC].apply()
      assert(second == (None, None, Some(Second(2, "b"))))

      val unused = Annotations[Unused, CC].apply()
      assert(unused == (None, None, None))

      val firstSum = Annotations[First, Base].apply()
      assert(firstSum == (Some(First()), None))

      val secondSum = Annotations[Second, Base].apply()
      assert(secondSum == (None, Some(Second(3, "e"))))
    }
  }

  @Test
  def invalidAnnotations: Unit = {
    illTyped(" Annotations[Abstract1, CC] ", ".*no implicit argument.*")
    illTyped(" Annotations[Abstract1, Base] ", ".*no implicit argument.*")
    illTyped(" Annotations[Abstract2, CC] ", ".*no implicit argument.*")
    illTyped(" Annotations[Abstract2, Base] ", ".*no implicit argument.*")
    illTyped(" Annotations[Second, Abstract1] ", ".*no implicit argument.*")
  }
}

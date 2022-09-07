/*
 * Copyright (c) 2015-6 Alexandre Archambault
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
import shapeless.test.{illTyped, typed}

object AnnotationTestsDefinitions {

  case class First() extends saAnnotation
  case class Second(i: Int, s: String) extends saAnnotation
  case class Third(c: Char) extends saAnnotation
  case class Fourth[T](t: T) extends saAnnotation

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
  @Second(3, "e") @Third('c') case class BaseS(s: String) extends Base

  sealed trait Base2
  case class BaseI2(i: Int) extends Base2 @First
  case class BaseS2(s: String) extends Base2 @Second(3, "e") @Third('c')

  trait Dummy

  case class CC2(
    i: Int @First,
    s: String,
    ob: Option[Boolean] @Second(2, "b")
  )

  case class CC3(
    @First i: Int,
    s: String,
    @Second(2, "b") @Third('c') ob: Option[Boolean],
    @Fourth(4) c: Char
  )
  
  case class CC4(
    i: Int @First,
    s: String,
    ob: Option[Boolean] @Second(2, "b") @Third('c'),
    c: Char @Fourth(4)
  )

  type PosInt = Int @First
  type Email = String @Third('c')
  case class User(age: PosInt, email: Email)
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
  def optionalAnnotation: Unit = {
    {
      val other = Annotation[Option[Other], CC].apply()
      assert(other == Some(Other()))

      val last = Annotation[Option[Last], Something].apply()
      assert(last == Some(Last(true)))
    }

    {
      val other: Option[Other] = Annotation[Option[Other], Something].apply()
      assert(other == None)

      val last: Option[Last] = Annotation[Option[Last], CC].apply()
      assert(last == None)
    }
  }


  @Test
  def invalidAnnotation: Unit = {
    illTyped(" Annotation[Other, Dummy] ", "could not find implicit value for parameter annotation: .*")
    illTyped(" Annotation[Dummy, CC] ", "could not find implicit value for parameter annotation: .*")
  }

  @Test
  def simpleAnnotations: Unit = {
    {
      val first: Some[First] :: None.type :: None.type :: HNil = Annotations[First, CC].apply()
      assert(first == Some(First()) :: None :: None :: HNil)

      val second: None.type :: None.type :: Some[Second] :: HNil = Annotations[Second, CC].apply()
      assert(second == None :: None :: Some(Second(2, "b")) :: HNil)

      val fourth: None.type :: None.type :: None.type :: Some[Fourth[Int]] :: HNil = Annotations[Fourth[_], CC3].apply()
      assert(fourth == None :: None :: None :: Some(Fourth(4)) :: HNil)

      val unused: None.type :: None.type :: None.type :: HNil = Annotations[Unused, CC].apply()
      assert(unused == None :: None :: None :: HNil)

      val firstSum: Some[First] :: None.type :: HNil = Annotations[First, Base].apply()
      assert(firstSum == Some(First()) :: None :: HNil)

      val secondSum: None.type :: Some[Second] :: HNil = Annotations[Second, Base].apply()
      assert(secondSum == None :: Some(Second(3, "e")) :: HNil)
    }

    {
      val first = Annotations[First, CC].apply()
      assert(first == Some(First()) :: None :: None :: HNil)

      val second = Annotations[Second, CC].apply()
      assert(second == None :: None :: Some(Second(2, "b")) :: HNil)

      val fourth = Annotations[Fourth[_], CC3].apply()
      assert(fourth == None :: None :: None :: Some(Fourth(4)) :: HNil)

      val unused = Annotations[Unused, CC].apply()
      assert(unused == None :: None :: None :: HNil)

      val firstSum = Annotations[First, Base].apply()
      assert(firstSum == Some(First()) :: None :: HNil)

      val secondSum = Annotations[Second, Base].apply()
      assert(secondSum == None :: Some(Second(3, "e")) :: HNil)
    }
  }

  @Test
  def invalidAnnotations: Unit = {
    illTyped(" Annotations[Dummy, CC] ", "could not find implicit value for parameter annotations: .*")
    illTyped(" Annotations[Dummy, Base] ", "could not find implicit value for parameter annotations: .*")
    illTyped(" Annotations[Second, Dummy] ", "could not find implicit value for parameter annotations: .*")
  }

  @Test
  def typeAnnotations: Unit = {
    {
      val first: Some[First] :: None.type :: None.type :: HNil = TypeAnnotations[First, CC2].apply()
      assert(first == Some(First()) :: None :: None :: HNil)

      val second: None.type :: None.type :: Some[Second] :: HNil = TypeAnnotations[Second, CC2].apply()
      assert(second == None :: None :: Some(Second(2, "b")) :: HNil)

      val fourth: None.type :: None.type :: None.type :: Some[Fourth[Int]] :: HNil = TypeAnnotations[Fourth[_], CC4].apply()
      assert(fourth == None :: None :: None :: Some(Fourth(4)) :: HNil)

      val unused: None.type :: None.type :: None.type :: HNil = TypeAnnotations[Unused, CC2].apply()
      assert(unused == None :: None :: None :: HNil)
    }

    {
      val first = TypeAnnotations[First, CC2].apply()
      assert(first == Some(First()) :: None :: None :: HNil)

      val second = TypeAnnotations[Second, CC2].apply()
      assert(second == None :: None :: Some(Second(2, "b")) :: HNil)

      val fourth = TypeAnnotations[Fourth[_], CC4].apply()
      assert(fourth == None :: None :: None :: Some(Fourth(4)) :: HNil)

      val unused = TypeAnnotations[Unused, CC2].apply()
      assert(unused == None :: None :: None :: HNil)
    }
  }

  @Test
  def invalidTypeAnnotations: Unit = {
    illTyped(" TypeAnnotations[Dummy, CC2] ", "could not find implicit value for parameter annotations: .*")
    illTyped(" TypeAnnotations[Dummy, Base] ", "could not find implicit value for parameter annotations: .*")
    illTyped(" TypeAnnotations[Second, Dummy] ", "could not find implicit value for parameter annotations: .*")
  }

  @Test
  def allAnnotations: Unit = {
    val cc = AllAnnotations[CC3].apply()
    typed[(First :: HNil) :: HNil :: (Second :: Third :: HNil) :: (Fourth[Int] :: HNil) :: HNil](cc)
    assert(cc == (First() :: HNil) :: HNil :: (Second(2, "b") :: Third('c') :: HNil) :: (Fourth(4) :: HNil) :: HNil)

    val st = AllAnnotations[Base].apply()
    typed[(First :: HNil) :: (Second :: Third :: HNil) :: HNil](st)
  }
  
  @Test
  def allTypeAnnotations: Unit = {
    val st = AllTypeAnnotations[Base2].apply() // sealed trait
    typed[(First :: HNil) :: (Second :: Third :: HNil) :: HNil](st)

    val cc = AllTypeAnnotations[CC4].apply() // case class
    typed[(First :: HNil) :: HNil :: (Second :: Third :: HNil) :: (Fourth[Int] :: HNil) :: HNil](cc)
    assert(cc == (First() :: HNil) :: HNil :: (Second(2, "b") :: Third('c') :: HNil) :: (Fourth(4) :: HNil) :: HNil)

    val user = AllTypeAnnotations[User].apply() // type refs
    typed[(First :: HNil) :: (Third :: HNil) :: HNil](user)
    assert(user == (First() :: HNil) :: (Third('c') :: HNil) :: HNil)
  }
}

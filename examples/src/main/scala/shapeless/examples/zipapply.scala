/*
 * Copyright (c) 2016 Miles Sabin
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

package shapeless.examples

import java.util.UUID

import shapeless._
import shapeless.ops.hlist._

// Spoiler alert - don't look!
object compose extends Poly1 {
  implicit def cases[A, B, C] = at[(A => B, B => C)] {
    case (f1, f2) => f1 andThen f2
  }
}

/**
  * Examples for ZipApply
  *
  * @author Jeremy Smith
  */
object ZipApplyExamples extends App {

  // Some functions over various types
  def intFunction(i: Int): String = (i * i).toString
  def stringFunction(s: String): Boolean = s startsWith "4"
  def longFunction(l: Long): Int = (l & 0xFFFFFFFF).toInt
  def uuidFunction(u: UUID): Long = u.getLeastSignificantBits ^ u.getMostSignificantBits
  def booleanFunction(b: Boolean): String = if(b) "Yes" else "No"

  // Just to illustrate the types of the HLists - you don't actually need to define type aliases at all.
  type Functions = (Int => String) :: (String => Boolean) :: (Long => Int) :: (UUID => Long) :: HNil
  type Args      = Int :: String :: Long :: UUID :: HNil
  type Results   = String :: Boolean :: Int :: Long :: HNil

  // Some sample values
  val anInt = 22
  val aString = "foo"
  val aLong = 33L
  val aUUID = UUID.fromString("deadbeef-dead-dead-beef-deaddeadbeef")

  // Example values of those types
  val functions1 = (intFunction _) :: (stringFunction _) :: (longFunction _) :: (uuidFunction _) :: HNil
  val args1      = anInt :: aString :: aLong :: aUUID :: HNil
  val results1   = intFunction(anInt) :: stringFunction(aString) :: longFunction(aLong) :: uuidFunction(aUUID) :: HNil

  // Just to show that the types line up
  implicitly[functions1.type <:< Functions]
  implicitly[args1.type <:< Args]
  implicitly[results1.type <:< Results]

  // A different type of HList of functions (it's specially planned - wait for the big twist!)
  val functions2 = (stringFunction _) :: (booleanFunction _) :: (intFunction _) :: (longFunction _) :: HNil

  /**
    * [[ZipApply]] allows application of an [[HList]] of functions to an [[HList]] of their corresponding arguments,
    * resulting in an [[HList]] of the results of the applications. Here's an example.
    */
  def zipApplyExample() = {

    // At a minimum, ZipApply requires the argument types to be known
    val zipApply = ZipApply[Functions, Args]
    val results  = zipApply(functions1, args1)

    // The result type lines up
    implicitly[results.type <:< Results]

    // The results are what's expected
    assert(results == results1)

    /**
      * The result type can also be inferred at a function call site, using ZipApply.Aux — this allows you to use other
      * ops with type inference in between. Here's a neat function that will take two functions of HLists and compose
      * each element. It also takes an HLists of arguments, which must be compatible with the first HList of functions.
      * The composed functions are applied to the arguments, and we get an HList of the results of those. If you try
      * to call this function on two HLists of functions that won't compose, or with arguments that don't map to the
      * first HList of functions, you'll get a compile time error at that call site. Isn't it great?
      *
      * The arguments:
      * @param f1 An HList of functions
      * @param f2 Another HList of functions — each element must be composable with the corresponding element of f1
      * @param a  An HList of arguments — each element must be of the input type of the corresponding function from f1
      *
      * The implicits used:
      * @param zip        A [[Zip]] instance which allows us to zip f1 and f2, giving an HList with the corresponding
      *                   elements from each f1 and f2 as a [[Tuple2]] - i.e.
      *                        (f1.head, f2.head) :: (f1.drop(1).head, f2.drop(1).head) ... :: HNil
      * @param mapCompose A [[Mapper]] instance which allows us to map the output of `zip` with the [[compose]] poly
      *                   function (defined at the bottom of this file)
      * @param zipApply   A [[ZipApply]] instance which will perform the application of the composed functions. Note the
      *                   use of `Aux` to infer the result type.
      *
      * And the type parameters:
      * @tparam Functions1 The type of the first [[HList]] of functions. This can be inferred because `inferenceExample`
      *                    takes an argument of this type.
      * @tparam Functions2 The type of the second [[HList]] of functions. This can be inferred for the same reason.
      * @tparam Arguments  The type of the [[HList]] of arguments. Again, this can be inferred since it must be concrete
      *                    at the call site.
      * @tparam FCombined  The type of the [[HList]] of pairs zipped from [[Functions1]] and [[Functions2]]. This gets
      *                    inferred thanks to the use of [[Zip.Aux]].
      * @tparam FComposed  The type of the [[HList]] of composed functions from [[Functions1]] and [[Functions2]]. This
      *                    gets inferred thanks to the use of [[Mapper.Aux]].
      * @tparam Output     The type of the [[HList]] of results from applying [[FComposed]] to [[Arguments]]. This gets
      *                    inferred thanks to the use of [[ZipApply.Aux]].
      *
      * @return The results of applying the composed functions to the corresponding arguments, as an HList.
      */
    def inferenceExample[
      Functions1 <: HList,
      Functions2 <: HList,
      Arguments  <: HList,
      FCombined  <: HList,
      FComposed  <: HList,
      Output     <: HList
    ](
      f1: Functions1,
      f2: Functions2,
      a: Arguments)(implicit
      zip: Zip.Aux[Functions1 :: Functions2 :: HNil, FCombined],
      mapCompose: Mapper.Aux[compose.type, FCombined, FComposed],
      zipApply: ZipApply.Aux[FComposed, Arguments, Output]
    ): Output = zipApply(mapCompose(zip(f1 :: f2 :: HNil)), a)

    /**
      * An example invocation of [[inferenceExample]] — note that all of the type arguments are inferred.
      * Here's the big plot twist! That innocent looking [[HList]], [[functions2]], composes with [[functions1]]!
      * (cue Dramatic Hamster)
      */
    val inferenceResults = inferenceExample(functions1, functions2, args1)

    // We can show that the result type was correctly inferred
    implicitly[inferenceResults.type <:< (Boolean :: String :: String :: Int :: HNil)]

    // and that the results are what's expected
    val expected =
      stringFunction(intFunction(anInt)) ::
      booleanFunction(stringFunction(aString)) ::
      intFunction(longFunction(aLong)) ::
      longFunction(uuidFunction(aUUID)) ::
      HNil

    assert(inferenceResults == expected)

    // the following would not compile, because functions1 doesn't compose with itself
    // val inferenceResults2 = inferenceExample(functions1, functions1, args1)

    // the following would not compile, because the argument types are wrong
    // val inferenceResults3 = inferenceExample(functions1, functions2, "hey" :: "hi" :: "how ya" :: "doin'" :: HNil)

  }


}

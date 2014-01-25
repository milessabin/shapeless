/*
 * Copyright (c) 2014 Miles Sabin 
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

import shapeless._
import record._
import syntax.singleton._

/**
 * Proof of concept translation of some Scalding examples[1] to shapeless
 * records in response to @posco's challenge on Twitter[2]. See also
 * comments from @deanwampler here[3].
 *
 * [1] https://github.com/twitter/scalding/wiki/Fields-based-API-Reference
 * [2] https://twitter.com/scalding/status/425704713447682048
 * [3] https://gist.github.com/milessabin/8549878
 * 
 * @author Miles Sabin
 */
object ScaldingPoC extends App {

  // map, flatMap etc.
  val birds =
    List(
      "name" ->> "Swallow (European, unladen)" :: "speed" ->> 23 :: "weightLb" ->> 0.2 :: "heightFt" ->> 0.65 :: HNil,
      "name" ->> "African (European, unladen)" :: "speed" ->> 24 :: "weightLb" ->> 0.21 :: "heightFt" ->> 0.6 :: HNil
    )

  val fasterBirds = birds.map(b => b + ("doubleSpeed" ->> b("speed")*2))
  fasterBirds foreach println

  val britishBirds = birds.map(b => b + ("weightKg" ->> b("weightLb")*0.454) + ("heightM" ->> b("heightFt")*0.305))
  britishBirds foreach println

  val items =
    List(
      "author" ->> "Benjamin Pierce" :: "title"  ->> "Types and Programming Languages" :: "price"  ->>  49.35 :: HNil,
      "author" ->> "Roger Hindley" :: "title"  ->> "Basic Simple Type Theory" :: "price"  ->> 23.14 :: HNil
    )

  val pricierItems = items.map(i => i + ("price" ->> i("price")*1.1))
  pricierItems foreach println

  val books =
    List(
      "text" ->> "Not everyone knows how I killed old Phillip Mathers" :: HNil,
      "text" ->> "No, no, I can't tell you everything" :: HNil
    )

  val lines = books.flatMap(book => for(word <- book("text").split("\\s+")) yield book + ("word" ->> word))
  lines foreach println
}  

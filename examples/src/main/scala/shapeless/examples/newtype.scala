/*
 * Copyright (c) 2011 Miles Sabin 
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

object NewtypeExampes extends App {
  import shapeless._
  import TypeOperators._
  
  // MyString is a new type with String as its underlying representation and with its operations 
  // provided by MyStringOps
  type MyString = Newtype[String, MyStringOps]
  
  // MyString constructor
  def MyString(s : String) : MyString = newtype(s)
  
  // Expose String#size as MyString#mySize. No other operations of String are accessible
  case class MyStringOps(s : String) {
    def mySize = s.size
  }
  implicit val mkOps = MyStringOps
  
  val ms = MyString("foo")
  
  //ms.size    // Does not compile
  ms.mySize  // Compiles. Result is 3.
  
  val s2 = "bar"
  val ms2 = MyString(s2)
  
  // Verify that this is an unboxed representation
  assert(ms2 eq s2)
}

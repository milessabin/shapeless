/*
 * Copyright (c) 2015-16 Miles Sabin
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
package test

import scala.language.experimental.macros

import scala.reflect.macros.blackbox

class TypeTrace[T]

object TypeTrace {
  implicit def apply[T]: TypeTrace[T] = macro TypeTraceMacros.applyImpl[T]
}

class TypeTraceMacros(val c: blackbox.Context) {
  import c.universe._

  def applyImpl[T](implicit tTag: WeakTypeTag[T]): Tree = {
    val tTpe = weakTypeOf[T]
    println(s"Trace: $tTpe ${tTpe.dealias} ${tTpe.getClass.getName} ${tTpe.dealias.getClass.getName}")

    q"""new _root_.shapeless.test.TypeTrace[$tTpe]"""
  }
}

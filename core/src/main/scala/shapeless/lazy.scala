/*
 * Copyright (c) 2013-4 Miles Sabin
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

import scala.language.experimental.macros

import scala.reflect.macros.blackbox

trait Lazy[T] {
  val value: T

  def map[U](f: T => U): Lazy[U] = Lazy { f(value) }
  def flatMap[U](f: T => Lazy[U]): Lazy[U] = Lazy { f(value).value }
}

object Lazy {
  implicit def apply[T](t: => T): Lazy[T] = new Lazy[T] {
    lazy val value = t
  }

  def unapply[T](lt: Lazy[T]): Option[T] = Some(lt.value)

  implicit def mkLazy[T]: Lazy[T] = macro mkLazyImpl[T]

  def mkLazyImpl[T: c.WeakTypeTag](c: blackbox.Context): c.Tree = {
    import c.universe._

    val tpe = weakTypeOf[T]

    val className = TypeName(c.freshName)
    val recName = TermName(c.freshName)

    q"""
      {
        final class $className extends Lazy[$tpe] {
          implicit val $recName: Lazy[$tpe] = this   // implicit self-publication ties the knot
          lazy val value: $tpe = implicitly[$tpe]
        }
        new $className
      }
    """
  }
}

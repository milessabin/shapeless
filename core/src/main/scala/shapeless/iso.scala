/*
 * Copyright (c) 2012-13 Miles Sabin 
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

/**
 * Representation of an isomorphism between a type (typically a case class) and an `HList`.
 */
@deprecated("Use Generic, Generic.Aux or scalaz Iso instead", "2.0.0")
trait Iso[T, U] { self =>
  def to(t : T) : U
  def from(u : U) : T

  def reverse : Iso[U, T] = new Iso[U, T] {
    def to(u : U) : T = self.from(u)
    def from(t : T) : U = self.to(t)

    override def reverse = self
  }
}

object Iso {
  def apply[T, U](implicit iso: Iso[T, U]) = iso
  
  implicit def materializeFromGeneric[T](implicit gen: Generic[T]): Iso[T, gen.Repr] = new Iso[T, gen.Repr] {
    def to(t : T) : gen.Repr = gen.to(t)
    def from(u : gen.Repr) : T = gen.from(u)
  } 
}

// vim: expandtab:ts=2:sw=2

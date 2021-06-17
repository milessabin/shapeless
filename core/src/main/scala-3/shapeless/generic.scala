/*
 * Copyright (c) 2012-18 Lars Hupel, Miles Sabin
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

trait GenericScalaCompat {

  implicit def materialize[T, R]: Generic.Aux[T, R] = ???
}

trait LabelledGenericScalaCompat {

  implicit def materialize[T, R]: LabelledGeneric.Aux[T, R] = ???
}

trait IsTupleScalaCompat {
  implicit def apply[T]: IsTuple[T] = ???
}

trait HasProductGenericScalaCompat {
  implicit def apply[T]: HasProductGeneric[T] = ???
}

trait HasCoproductGenericScalaCompat {
  implicit def apply[T]: HasCoproductGeneric[T] = ???
}

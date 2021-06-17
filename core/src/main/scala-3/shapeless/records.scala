/*
 * Copyright (c) 2011-16 Miles Sabin
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

trait RecordScalaCompat {
  def applyDynamic(method: String)(rec: Any*): HList = ???
  def applyDynamicNamed(method: String)(rec: Any*): HList = ???
  def selectDynamic(tpeSelector: String): Any = ???
}

trait RecordArgsScalaCompat {
  def applyDynamic(method: String)(): Any = ???
  def applyDynamicNamed(method: String)(rec: Any*): Any = ???
}

trait FromRecordArgsScalaCompat {
  def applyDynamic[L <: HList](method: String)(rec: L): Any = ???
}

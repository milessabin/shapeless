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

package shapeless

/*
 * Workaround for Scala 2.10 consle macro classloader bug. See,
 *
 *  https://github.com/milessabin/shapeless/issues/367
 *  https://github.com/milessabin/shapeless/issues/547
 *
 * See corresponding definition for 2.10
 */
object LazyMacrosRef {
  def deriveInstance(lm: LazyMacros)(tpe: lm.c.Type, mkInst: (lm.c.Tree, lm.c.Type) => lm.c.Tree): lm.c.Tree =
    LazyMacros.deriveInstance(lm)(tpe, mkInst)
}

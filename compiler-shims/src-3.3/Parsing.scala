/*
 * Copyright (c) 2026 Miles Sabin
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

// The build compiles this source against compilers from Scala 3.3 to 3.6 (see `compilerShim` in build.sbt)
package shapeless.compilershims

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.parsing.Parsers.Parser
import dotty.tools.dotc.util.SourceFile

object Parsing {
  /**
   * Parses `source` as a compilation unit. The parser phase isn't used, as the type of its `parse` method differs
   * between Scala 3.3.0 and later 3.3 versions.
   */
  def parseUnit(source: SourceFile)(using Context): untpd.Tree = Parser(source).parse()
}

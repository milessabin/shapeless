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

// The build compiles this source against compilers from Scala 3.7 (see `compilerShim` in build.sbt)
package shapeless.compilershims

import dotty.tools.dotc.CompilationUnit
import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.{ctx, Context}
import dotty.tools.dotc.parsing.Parser
import dotty.tools.dotc.util.SourceFile

object Parsing {
  /**
   * Parses `source` as a compilation unit. The parser phase is used, as the constructor of `Parsers.Parser` differs
   * between Scala 3.7 and 3.10.
   */
  def parseUnit(source: SourceFile)(using Context): untpd.Tree = {
    val unit = CompilationUnit(source, false)
    Parser().parse(using ctx.fresh.setCompilationUnit(unit))
    unit.untpdTree
  }
}

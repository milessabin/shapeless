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

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts.{ctx, Context}
import dotty.tools.dotc.core.Symbols.Symbol
import dotty.tools.dotc.core.Types.Type
import dotty.tools.dotc.util.Spans.Span

object ImplicitSearches {
  /** As `Typer.inferImplicitArg`, except that the implicit definitions `ignored` aren't candidates. */
  def inferImplicitArgIgnoring(pt: Type, span: Span, ignored: Set[Symbol])(using Context): tpd.Tree =
    ctx.typer.inferImplicitArg(pt, span, ignored)
}

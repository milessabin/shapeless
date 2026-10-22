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

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts.{ctx, Context}
import dotty.tools.dotc.core.Symbols.Symbol
import dotty.tools.dotc.core.Types.Type
import dotty.tools.dotc.typer.Synthesizer
import dotty.tools.dotc.typer.Implicits.{
  Candidate, NoMatchingImplicits, NoMatchingImplicitsFailure, SearchFailure, SearchResult, SearchSuccess, SynthesisFailure
}
import dotty.tools.dotc.util.Spans.Span

object ImplicitSearches {
  /**
   * As `Typer.inferImplicitArg`, except that the implicit definitions `ignored` aren't candidates, as from Scala 3.7.
   */
  def inferImplicitArgIgnoring(pt: Type, span: Span, ignored: Set[Symbol])(using Context): tpd.Tree = {
    val typer = ctx.typer
    val search = new typer.ImplicitSearch(pt, tpd.EmptyTree, span) {
      override def tryImplicit(cand: Candidate, contextual: Boolean): SearchResult =
        if (ignored.contains(cand.implicitRef.underlyingRef.symbol)) NoMatchingImplicitsFailure
        else super.tryImplicit(cand, contextual)
    }
    search.bestImplicit match {
      case found: SearchSuccess =>
        if (found.tstate ne ctx.typerState) found.tstate.commit()
        if (found.gstate ne ctx.gadt) ctx.gadtState.restore(found.gstate)
        // As `Typer.inferImplicit`: at the outermost implicit search, define the recursive by-name implicits the value
        // found refers to (eg. `$_lazy_implicit_$1`), which would otherwise be undefined
        ctx.searchHistory.emitDictionary(span, found) match {
          case success: SearchSuccess => success.tree
          case failure: SearchFailure => failure.tree
        }
      case failed: SearchFailure if failed.isAmbiguous => failed.tree
      case failed: SearchFailure =>
        val reported =
          if (failed eq NoMatchingImplicitsFailure)
            SearchFailure(new NoMatchingImplicits(pt, tpd.EmptyTree, ctx.typerState.constraint), span)
          else failed
        val (synthesized, errors) = Synthesizer(typer).tryAll(pt, span)
        if (errors.nonEmpty) SearchFailure(new SynthesisFailure(errors, pt), span).tree
        else synthesized.orElse(reported.tree)
    }
  }
}

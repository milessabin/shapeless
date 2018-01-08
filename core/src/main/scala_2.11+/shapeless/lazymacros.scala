/*
 * Copyright (c) 2013-17 Miles Sabin
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

// Horrible hack to fix breakage in 2.12.4/2.13.x without either breaking 2.10.x or forcing
// an update to macro-compat to support Tree attachments.
trait LazyMacrosCompat {
  def dcRef(lm: LazyMacros): Option[LazyMacros#DerivationContext] = {
    // N.B. openMacros/enclosingMacros annoyingly include macros which are not enclosing this macro at all,
    // but simply happen to be expanding further up on the same compiler stack (and the compiler stack doesn't
    // necessarily correspond to a single path through the AST - it can jump to other trees during typing), so
    // we need to stop once the position of the open macros no longer matches ours
    lm.c.openMacros.takeWhile(_.enclosingPosition == lm.c.enclosingPosition)
      // use the first enclosing DerivationContext we find (if any)
      .find(c => c.internal.attachments(c.macroApplication).contains[lm.DerivationContext])
      .flatMap(c => c.internal.attachments(c.macroApplication).get[lm.DerivationContext])
  }

  def deriveInstance(lm: LazyMacros)(tpe: lm.c.Type, mkInst: (lm.c.Tree, lm.c.Type) => lm.c.Tree): lm.c.Tree = {
    val (dc, root) =
      dcRef(lm) match {
        case None =>
          lm.resetAnnotation
          val dc = new lm.DerivationContext
          lm.c.internal.updateAttachment(lm.c.macroApplication, dc)
          (dc, true)
        case Some(dc) =>
          (dc.asInstanceOf[lm.DerivationContext], false)
      }

    if (root)
      // Sometimes corrupted, and slows things too
      lm.c.universe.asInstanceOf[scala.tools.nsc.Global].analyzer.resetImplicits()

    try {
      dc.State.deriveInstance(tpe, root, mkInst)
    } finally {
      if(root) {
        lm.c.internal.removeAttachment[lm.DerivationContext](lm.c.macroApplication)
      }
    }
  }
}


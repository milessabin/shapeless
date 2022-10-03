package shapeless

import scala.reflect.{ClassTag, classTag}
import scala.reflect.macros.Universe


private[shapeless] object MacroState {
  private class MacroStateAttachment(val state: collection.mutable.HashMap[Class[_], Any])

  /**
   * Associates some state with a global.Run. Preferable to using static state in the macro
   * classloader (e.g vars in top level objects) so as to avoid race conditions under `-Ycache-macro-classloader`.
   *
   * @tparam T the type of the state. The erased `Class[_]` value for this type will be used a a map key, so each user of
   *           this facility should use a dedicated and distinct class to wrap the state.
   * @param u The reflection universe, typically obtained from `context.universe` in a macro implementation.
   * @param factory Factory to create the state.
   */
  def getOrElseUpdate[T: ClassTag](u: Universe, factory: => T): T = {
    // Cast needed for access to perRunCaches and convenient access to attachments
    val g = u.asInstanceOf[scala.reflect.internal.SymbolTable]

    // Sneakily use a symbol attachment on a well-known symbol to hold our map of macro states.
    // Compiler plugins would typically use `val someState = global.perRunCaches.newMap` in a `Component` that
    // is instantiated once per Global, but macros don't have an analagous place.
    //
    // An alternative would be a `Map[Universe, MacroState]`, but this would need to be carefully constructed
    // with weak references to avoid leaking the `Universe` through the key or values.
    val holderSymbol = g.definitions.AnyClass

    // The erasure of `T` is the key for our internal map
    val cls = classTag[T].runtimeClass

    // classOf[MacroStateAttachment] is the key for the attachment lookup.
    holderSymbol.attachments.get[MacroStateAttachment] match {
      case Some(existing) =>
        existing.state.getOrElseUpdate(cls, factory).asInstanceOf[T]
      case None =>
        val value = factory
        // Use perRunCaches.newMap to clear this map before the next Run starts.
        // The REPL or presentation compiler use a single Global to perform multiple compilation Runs.
        val macroState = new MacroStateAttachment(g.perRunCaches.newMap())
        macroState.state.put(cls, value)
        holderSymbol.updateAttachment(macroState)
        value
    }
  }
}

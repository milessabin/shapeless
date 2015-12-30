package shapeless

import scala.language.experimental.macros
import scala.reflect.macros.whitebox

/**
 * Wraps a cached implicit `T`.
 *
 * Looking for an implicit `Cached[T]` first triggers a look for an implicit `T`, caches the resulting
 * tree, and returns it immediately and in subsequent look ups for an implicit `Cached[T]`. Thus,
 * subsequent look ups do not trigger looking for an implicit `T`, only returning the instance kept in
 * cache.
 *
 * Beware that if the contexts in which two subsequent look ups are different, so that looking for a
 * `T` in each of them doesn't return the same result, this change would be ignored by caching. Looking
 * for a `Cached[T]` in the first context would put the implicit `T` of this context in cache, and then
 * looking for a `Cached[T]` in the second context would return the former instance from the first
 * context. E.g.
 *
 * {{{
 *   trait TC[T] {
 *     def msg: String
 *   }
 *
 *   object First {
 *     implicit val tc: TC[Int] = new TC[Int] {
 *       val msg = "first"
 *     }
 *
 *     def print() = println(implicitly[TC[Int]].msg)
 *     def printCached() = println(cached[TC[Int]].msg)
 *   }
 *
 *   object Second {
 *     implicit val tc: TC[Int] = new TC[Int] {
 *       val msg = "second"
 *     }
 *
 *     def print() = println(implicitly[TC[Int]].msg)
 *     def printCached() = println(cached[TC[Int]].msg)
 *   }
 *
 *   First.print()
 *   Second.print()
 *   First.printCached()
 *   Second.printCached()
 * }}}
 *
 * would print "first" then "second" (non cached `TC[Int]` instances), then "first" twice (first instance, returned
 * the second time too through the cache).
 *
 * @author Alexandre Archambault
 */
case class Cached[+T](value: T) extends AnyVal

object Cached {
  implicit def materialize[I]: Cached[I] = macro CachedMacros.materializeCached[I]

  def implicitly[T](implicit cached: Cached[T]): T = cached.value
}

object CachedMacros {
  var deriving = false
  var cache = List.empty[(Any, Any)]
}

class CachedMacros(override val c: whitebox.Context) extends LazyMacros(c) {
  import c.universe._
  import c.ImplicitCandidate

  def materializeCached[T: WeakTypeTag]: Tree = {
    // Getting the actual type parameter T, using the same trick as Lazy/Strict
    val tpe = (c.openImplicits.headOption, weakTypeOf[T]) match {
      case (Some(ImplicitCandidate(_, _, TypeRef(_, _, List(tpe)), _)), _) =>
        tpe.map(_.dealias)
      case (None, tpe) =>                                     // Non-implicit invocation
        tpe
      case _ =>
        c.abort(c.enclosingPosition, s"Bad Cached materialization ${c.openImplicits.head}")
    }

    val concurrentLazy = !CachedMacros.deriving && LazyMacros.dcRef.nonEmpty

    // Ensuring we are not caching parts of trees derived during a Lazy/Strict lookup
    // (but caching the full tree of a Lazy/Strict is fine), as these can reference values
    // (other entries of the Lazy/Strict derivation) that should not be accessible if
    // re-using the tree in other contexts, after caching.
    if (concurrentLazy)
      c.warning(c.enclosingPosition,
        s"Cached[$tpe] called from a Lazy/Strict, you might want to consider caching " +
        "an implicit earlier, so that the whole Lazy/Strict itself gets cached. Caching " +
        "is disabled here."
      )

    if (CachedMacros.deriving || concurrentLazy) {
      // Caching only the first (root) Cached, not subsequent ones as here
      val tree0 = c.inferImplicitValue(tpe)
      if (tree0 == EmptyTree)
        c.abort(c.enclosingPosition, s"Implicit $tpe not found")
      q"_root_.shapeless.Cached($tree0)"
    } else {
      CachedMacros.deriving = true

      try {
        val treeOpt = CachedMacros.cache.asInstanceOf[List[(Type, Tree)]].collectFirst {
          case (eTpe, eTree) if eTpe =:= tpe => eTree
        }

        treeOpt.getOrElse {
          // Cached instances are derived like Lazy or Strict instances.
          // Trying to derive them in a standalone way raised
          // https://github.com/fommil/spray-json-shapeless/issues/14.
          val tree = mkImpl[T](
            (tree, actualType) => q"_root_.shapeless.Cached[$actualType]($tree)",
            q"null.asInstanceOf[_root_.shapeless.Cached[_root_.scala.Nothing]]"
          )

          CachedMacros.cache = (tpe -> tree) :: CachedMacros.cache
          tree
        }
      } finally {
        CachedMacros.deriving = false
      }
    }
  }

}

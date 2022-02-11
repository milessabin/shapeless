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
 *     def printCached() = println(Cached.implicitly[TC[Int]].msg)
 *   }
 *
 *   object Second {
 *     implicit val tc: TC[Int] = new TC[Int] {
 *       val msg = "second"
 *     }
 *
 *     def print() = println(implicitly[TC[Int]].msg)
 *     def printCached() = println(Cached.implicitly[TC[Int]].msg)
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

class CachedMacros(override val c: whitebox.Context) extends LazyMacros(c) with OpenImplicitMacros {
  import c.universe._

  def deepCopyTree(t: Tree): Tree = {
    val treeDuplicator = new Transformer {
      // by default Transformers don’t copy trees which haven’t been modified,
      // so we need to use use strictTreeCopier
      override val treeCopy =
        c.asInstanceOf[reflect.macros.runtime.Context].global.newStrictTreeCopier.asInstanceOf[TreeCopier]
    }

    treeDuplicator.transform(t)
  }

  def materializeCached[T: WeakTypeTag]: Tree = {
    // Getting the actual type parameter T, using the same trick as Lazy/Strict
    val tpe = openImplicitTpeParam.getOrElse(weakTypeOf[T])

    val concurrentLazy = !CachedMacros.deriving && LazyMacros.dcRef(this).nonEmpty

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

        deepCopyTree(treeOpt.getOrElse {
          // Cached instances are derived like Lazy or Strict instances.
          // Trying to derive them in a standalone way raised
          // https://github.com/fommil/spray-json-shapeless/issues/14.
          val tree0 = mkImpl[T](
            (tree, actualType) => q"_root_.shapeless.Cached[$actualType]($tree)",
            q"null.asInstanceOf[_root_.shapeless.Cached[_root_.scala.Nothing]]"
          )
          val tree = c.untypecheck(tree0)

          CachedMacros.cache = (tpe -> tree) :: CachedMacros.cache
          tree
        })
      } finally {
        CachedMacros.deriving = false
      }
    }
  }

}

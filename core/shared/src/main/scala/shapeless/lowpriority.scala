package shapeless

import scala.language.experimental.macros
import scala.reflect.macros.whitebox

/**
 * Evidence that no implicit instance of the same type as the one being currently searched is
 * available elsewhere.
 *
 * Added to an implicit def like
 * {{{
 * implicit def genericThing[F, G]
 *  (implicit
 *    ev: LowPriority,
 *    gen: Generic.Aux[F, G],
 *    underlying: Thing[G]
 *  ): Thing[F] = ???
 * }}}
 * it prevents `genericThing` to provide an instance of `Thing[F]` if an implicit one is already
 * available elsewhere. This effectively gives `genericThing` a lower priority than the already
 * existing implicits - without having to deal with cumbersome priority scoping.
 *
 * @author Alexandre Archambault
 */
sealed trait LowPriority extends Serializable

object LowPriority {

  implicit def materialize: LowPriority = macro LowPriorityMacros.mkLowPriority


  /**
    * Allows `LowPriority` to ignore some implicits.
    *
    * Added to an implicit def like
    * {{{
    * implicit def genericThing[F, G]
    *  (implicit
    *    ev: LowPriority.Ignoring[Witness.`"anyThing"`.T],
    *    gen: Generic.Aux[F, G],
    *    underlying: Thing[G]
    *  ): Thing[F] = ???
    * }}}
    * it prevents `genericThing` to provide an instance of `Thing[F]` if an implicit one is already
    * available elsewhere EXCEPT if it is provided by a method called `anyThing`. In this case, it
    * lets `genericThing` provide a `Thing[F]` too, effectively taking precedence over the instances
    * provided by `anyThing`.
    *
    * @author Alexandre Archambault
    */
  sealed trait Ignoring[T] extends Serializable

  object Ignoring {
    implicit def materialize[T]: Ignoring[T] = macro LowPriorityMacros.mkLowPriorityIgnoring[T]
  }

  /** For internal use by `LowPriority` */
  trait For[T] extends Serializable
  /** For internal use by `LowPriority` */
  trait ForIgnoring[I, T] extends Serializable

}

class LowPriorityMacros(val c: whitebox.Context) extends OpenImplicitMacros with LowPriorityTypes {
  import c.universe._

  def strictTpe = typeOf[Strict[_]].typeConstructor

  // FIXME Also in LazyMacros
  def stripRefinements(tpe: Type): Option[Type] =
    tpe match {
      case RefinedType(parents, _) => Some(parents.head)
      case _ => None
    }

  def mkLowPriority: Tree =
    secondOpenImplicitTpe match {
      case Some(tpe) =>
        c.inferImplicitValue(
          appliedType(strictTpe, appliedType(lowPriorityForTpe, stripRefinements(tpe.dealias).getOrElse(tpe))),
          silent = false
        )

        q"null: _root_.shapeless.LowPriority"

      case None =>
        c.abort(c.enclosingPosition, "Can't get looked for implicit type")
    }

  def mkLowPriorityIgnoring[T: WeakTypeTag]: Tree =
    secondOpenImplicitTpe match {
      case Some(tpe) =>
        c.inferImplicitValue(
          appliedType(strictTpe, appliedType(lowPriorityForIgnoringTpe, weakTypeOf[T], tpe)),
          silent = false
        )

        q"null: _root_.shapeless.LowPriority.Ignoring[${weakTypeOf[T]}]"

      case None =>
        c.abort(c.enclosingPosition, "Can't get looked for implicit type")
    }

}

trait LowPriorityTypes {
  val c: whitebox.Context

  import c.universe._


  def lowPriorityForTpe: Type =
    typeOf[LowPriority.For[_]].typeConstructor
  def lowPriorityForIgnoringTpe: Type =
    typeOf[LowPriority.ForIgnoring[_, _]].typeConstructor

  object LowPriorityFor {
    def unapply(tpe: Type): Option[(String, Type)] =
      tpe.dealias match {
        case TypeRef(_, cpdTpe, List(highTpe))
          if cpdTpe.asType.toType.typeConstructor =:= lowPriorityForTpe =>
          Some(("", highTpe))
        case TypeRef(_, cpdTpe, List(ConstantType(Constant(ignored: String)), tTpe))
          if cpdTpe.asType.toType.typeConstructor =:= lowPriorityForIgnoringTpe &&
            ignored.nonEmpty =>
          Some((ignored, tTpe))
        case _ =>
          None
      }
  }

}

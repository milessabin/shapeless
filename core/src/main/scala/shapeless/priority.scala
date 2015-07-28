package shapeless

import scala.language.experimental.macros
import scala.reflect.macros.whitebox

/**
 * Looking for an implicit `Lazy[Priority[H, L]]` will look up for an implicit `H`,
 * or, if it fails, for an implicit `L`, and wrap the result in a `Lazy[Priority[H, L]]`.
 *
 * It allows for definitions like
 *     implicit def mkTC[T]
 *      (implicit
 *        impl: Lazy[Implicit[
 *          TC[T],
 *          Fallback[T]
 *        ]]
 *      ): TC[T] = impl.value.fold(identity)(_.toTC)
 * which is looking for an implicit `TC[T]`, but at the same time providing one. Without `Priority`,
 * this would typically lead to stack overflows at runtime, as `mkTC` would find itself as a `TC[T]`,
 * and call itself without terminating. Thanks to `Priority`, the lookup for a `TC[T]` by `mkTC` will
 * ignore the implicit `TC[T]` it itself provides, hence only looking for `TC[T]` elsewhere. Note that
 * this does not prevent `mkTC` to provide `TC` instances the same way for other types during the search
 * for a `TC[T]` or `Fallback[T]`.
 */
trait Priority[+H, +L] extends Serializable {
  def fold[T](high: H => T)(low: L => T): T =
    this match {
      case Priority.High(h) => high(h)
      case Priority.Low(l) => low(l)
    }
}

object Priority extends LazyExtensionCompanion {
  case class High[+H](value: H) extends Priority[H, Nothing]
  case class Low[+L](value: L) extends Priority[Nothing, L]

  def apply[H, L](implicit lzPriority: Lazy[Priority[H, L]]): Priority[H, L] =
    lzPriority.value


  implicit def init[H, L]: Priority[H, L] = macro initImpl

  def instantiate(ctx0: DerivationContext) =
    new PriorityLookupExtension {
      type Ctx = ctx0.type
      val ctx: ctx0.type = ctx0
    }
}

/**
 * Allows to mask some high priority implicits in a `Priority[H, L]` lookup.
 *
 * Typical usage is when a fallback for type class `TC` is defined in its companion, like
 *     object TC {
 *       implicit def anyTC[T]: TC[T] = ...
 *     }
 *
 * If one looks for a `Priority[TC[T], Other[T]]`, then the lookup for a `TC[T]` will always
 * succeed, because of the `anyTC` case above. With `Mask`, one can instead look for
 * a ``Priority[Mask[W.`anyTC`.T, TC[T]], Other[T]]``. This `Priority` will first look up
 * for a `TC[T]`, but will not accept it being made by `anyTC[T]`. Else, it will lookup for
 * a `Other[T]`.
 */
trait Mask[M, T] extends Serializable {
  def value: T
}

object Mask {
  def apply[M, T](implicit lzMask: Lazy[Mask[M, T]]): Mask[M, T] = lzMask.value

  def mkMask[M, T](t: T): Mask[M, T] =
    new Mask[M, T] {
      val value = t
    }
}


trait PriorityTypes {
  type C <: whitebox.Context
  val c: C

  import c.universe._


  def highPriorityTpe: Type = typeOf[Priority.High[_]].typeConstructor
  def lowPriorityTpe: Type = typeOf[Priority.Low[_]].typeConstructor
  def priorityTpe: Type = typeOf[Priority[_, _]].typeConstructor

  object PriorityTpe {
    def unapply(tpe: Type): Option[(Type, Type)] =
      tpe.dealias match {
        case TypeRef(_, cpdTpe, List(highTpe, lowTpe))
          if cpdTpe.asType.toType.typeConstructor =:= priorityTpe =>
          Some(highTpe, lowTpe)
        case _ =>
          None
      }
  }

  def maskTpe: Type = typeOf[Mask[_, _]].typeConstructor

  object MaskTpe {
    def unapply(tpe: Type): Option[(Type, Type)] =
      tpe.dealias match {
        case TypeRef(_, cpdTpe, List(mTpe, tTpe))
          if cpdTpe.asType.toType.typeConstructor =:= maskTpe =>
          Some(mTpe, tTpe)
        case _ =>
          None
      }
  }

}

trait PriorityLookupExtension extends LazyExtension with PriorityTypes {
  type C = ctx.c.type
  lazy val c: C = ctx.c

  import ctx._
  import c.universe._

  case class ThisState(
    priorityLookups: List[TypeWrapper]
  ) {
    def addPriorityLookup(tpe: Type): ThisState =
      copy(priorityLookups = TypeWrapper(tpe) :: priorityLookups)
    def removePriorityLookup(tpe: Type): ThisState =
      copy(priorityLookups = priorityLookups.filter(_ != TypeWrapper(tpe)))
  }

  def id = "priority"

  def initialState = ThisState(Nil)

  def derivePriority(
    state: State,
    extState: ThisState,
    update: (State, ThisState) => State )(
    priorityTpe: Type,
    highInstTpe: Type,
    lowInstTpe: Type,
    mask: String
  ): Option[(State, Instance)] = {
    val high = {
      val extState1 = extState
        .addPriorityLookup(priorityTpe)
      val state1 = update(state, extState1)

      ctx.derive(state1)(highInstTpe)
        .right.toOption
        .flatMap{case (state2, inst) =>
          if (inst.inst.isEmpty)
            resolve0(state2)(highInstTpe)
              .map{case (s, tree, tpe) => (s, tree, tree, tpe) }
          else
            Some((state2, inst.ident, inst.inst.get, inst.actualTpe))
        }
        .filter {case (_, _, actualTree, _) =>
          mask.isEmpty || {
            actualTree match {
              case TypeApply(method, other) =>
                !method.toString().endsWith(mask)
              case _ =>
                true
            }
          }
        }
        .map{case (state2, tree0, _, actualTpe) =>
          val (tree, actualType) =
            if (mask.isEmpty)
              (tree0, actualTpe)
            else {
              val mTpe = internal.constantType(Constant(mask))
              (q"_root_.shapeless.Mask.mkMask[$mTpe, $actualTpe]($tree0)", appliedType(maskTpe, List(mTpe, actualTpe)))
            }

          val extState2 = extState1
            .removePriorityLookup(priorityTpe)

          (
            update(state2, extState2),
            q"_root_.shapeless.Priority.High[$actualType]($tree)",
            appliedType(highPriorityTpe, List(actualType))
          )
        }
    }

    def low =
      ctx.derive(state)(lowInstTpe)
        .right.toOption
        .map{case (state1, inst) =>
          (state1, q"_root_.shapeless.Priority.Low[${inst.actualTpe}](${inst.ident})", appliedType(lowPriorityTpe, List(inst.actualTpe)))
        }

    high.orElse(low) .map {case (state1, extInst, actualTpe) =>
      state1.closeInst(priorityTpe, extInst, actualTpe)
    }
  }

  def derive(
    state: State,
    extState: ThisState,
    update: (State, ThisState) => State )(
    tpe: Type
  ): Option[Either[String, (State, Instance)]] =
    tpe match {
      case PriorityTpe(highTpe, lowTpe) =>
        Some {
          if (extState.priorityLookups.contains(TypeWrapper(tpe)))
            Left(s"Not deriving $tpe")
          else
            state.lookup(tpe).left.flatMap { state0 =>
              val eitherHighTpeMask =
                highTpe match {
                  case MaskTpe(mTpe, tTpe) =>
                    mTpe match {
                      case ConstantType(Constant(mask: String)) if mask.nonEmpty =>
                        Right((tTpe, mask))
                      case _ =>
                        Left(s"Unsupported mask type: $mTpe")
                    }
                  case _ =>
                    Right((highTpe, ""))
                }

              eitherHighTpeMask.right.flatMap{case (highTpe, mask) =>
                derivePriority(state0, extState, update)(tpe, highTpe, lowTpe, mask)
                  .toRight(s"Unable to derive $tpe")
              }
            }
        }

      case _ => None
    }
}


package shapeless

import scala.language.experimental.macros
import scala.reflect.macros.whitebox

/**
 * Evidence that no implicit `T` instance is available elsewhere.
 *
 * The instance using the `LowPriority[T]` is ignored.
 *
 * Allows to prioritize implicits, for example
 *
 * {{{
 *   trait TC[T] {
 *     def prop: Boolean
 *   }
 *
 *   object TC {
 *     // TC[Int] available by default, with field `prop` true
 *     implicit val intTC: TC[Int] = new TC[Int] { def prop = true }
 *   }
 *
 *   // extra `TC[T]`, with field `prop` false
 *   implicit def extraTC[T](implicit ev: LowPriority[TC[T]]): TC[T] =
 *     new TC[T] { def prop = false }
 *
 *   // Already available instance `intTC` is still found, because `extraTC[Int]` requires a
 *   // `LowPriority[TC[Int]]`, that will refuse to materialize (because `LowPriority` is able to
 *   // know about the already available `intTC`.)
 *   assert(implicitly[TC[Int]].prop == true)
 *
 *   // `extraTC[String]` is found, as no other `TC[String]` can be found elsewhere
 *   assert(implicitly[TC[String]].prop == false)
 * }}}
 *
 * @author Alexandre Archambault
 */
trait LowPriority[T] extends Serializable

object LowPriority extends LazyExtensionCompanion {
  def apply[T](implicit nf: Strict[LowPriority[T]]): LowPriority[T] =
    nf.value


  def id = "low-priority"

  implicit def init[T]: LowPriority[T] = macro initImpl[LowPriority[T]]

  def instantiate(ctx0: DerivationContext): LazyExtension { type Ctx = ctx0.type } =
    new LowPriorityExtension {
      type Ctx = ctx0.type
      val ctx: ctx0.type = ctx0
    }
}

trait LowPriorityTypes {
  type C <: whitebox.Context
  val c: C

  import c.universe._


  def lowPriorityTpe: Type = typeOf[LowPriority[_]].typeConstructor

  object LowPriorityTpe {
    def unapply(tpe: Type): Option[Type] =
      tpe.dealias match {
        case TypeRef(_, cpdTpe, List(highTpe))
          if cpdTpe.asType.toType.typeConstructor =:= lowPriorityTpe =>
          Some(highTpe)
        case _ =>
          None
      }
  }

  def maskTpe: Type = typeOf[Mask[_, _]].typeConstructor

  object LowPriorityMaskTpe {
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

trait LowPriorityExtension extends LazyExtension with LowPriorityTypes {
  type C = ctx.c.type
  lazy val c: C = ctx.c

  import ctx._
  import c.universe._

  case class ThisState(
    /**
     * `LowPriority` types whose derivation must fail no matter what.
     *
     * In the initial lookup for a `LowPriority[T]`, a `T` will be looked for elsewhere.
     * During the latter search, `LowPriority[T]` will be in this list, so that further
     * derivations of `LowPriority[T]` will fail, effectively *preventing* the implicit that should
     * be given a lower priority to be found.
     */
    prevent: List[TypeWrapper]
  ) {
    def :+(tpe: Type): ThisState =
      copy(prevent = TypeWrapper(tpe) :: prevent)

    def allowed(tpe: Type): Either[String, Unit] =
      if (prevent.contains(TypeWrapper(tpe)))
        Left(s"Not deriving $tpe")
      else
        Right(())
  }

  def id = LowPriority.id

  def initialState = ThisState(Nil)

  def deriveLowPriority(
    state: State,
    extState: ThisState,
    update: (State, ThisState) => State )(
    wrappedTpe: Type,
    innerTpe: Type,
    mask: String
  ): (State, Instance) = {
    val tmpState = update(state, extState :+ wrappedTpe)

    val existingInstOpt = ctx.derive(tmpState)(innerTpe).right.toOption.flatMap {
      case (state2, inst) =>
        if (inst.inst.isEmpty)
          resolve0(state2)(innerTpe).map { case (_, tree, _) => tree }
        else
          Some(inst.inst.get)
    }

    val existingInstAvailable = existingInstOpt.exists { actualTree =>
      def masked = actualTree match {
        case TypeApply(method, other) => method.toString().endsWith(mask)
        case _ => false
      }

      mask.isEmpty || !masked
    }

    if (existingInstAvailable)
      c.abort(c.enclosingPosition, s"$innerTpe available elsewhere")
    else {
      val innerTpe0 =
        if (mask.isEmpty)
          innerTpe
        else
          appliedType(maskTpe, List(internal.constantType(Constant(mask)), innerTpe))

      val low = q"""
        new _root_.shapeless.LowPriority[$innerTpe0] {} : _root_.shapeless.LowPriority[$innerTpe0]
      """
      val lowTpe = appliedType(lowPriorityTpe, List(innerTpe0))

      state.closeInst(wrappedTpe, low, lowTpe)
    }
  }

  private def withMask(tpe: Type): Either[String, (Type, String)] =
    tpe match {
      case LowPriorityMaskTpe(mTpe, innerTpe0) =>
        mTpe match {
          case ConstantType(Constant(mask: String)) if mask.nonEmpty => Right((innerTpe0, mask))
          case _ => Left(s"Unsupported mask type: $mTpe")
        }
      case _ => Right((tpe, ""))
    }

  def derive(
    state0: State,
    extState: ThisState,
    update: (State, ThisState) => State )(
    instTpe: Type
  ): Option[Either[String, (State, Instance)]] =
    instTpe match {
      case LowPriorityTpe(innerTpe) =>
        val res =
          for {
            _ <- extState.allowed(instTpe).right
            state <- state0.lookup(instTpe).left
            tpeMask <- withMask(innerTpe).right
          } yield {
            val (tpe, mask) = tpeMask
            deriveLowPriority(state, extState, update)(instTpe, tpe, mask)
          }

        Some(res)

      case _ => None
    }
}


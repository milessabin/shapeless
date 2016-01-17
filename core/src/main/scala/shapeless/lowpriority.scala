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

/**
 * Allows to ignore some implicits in a `LowPriority[T]`.
 *
 * Use like ``LowPriority[Ignoring[Witness.`"ignoredMethod"`.T, T]]``.
 *
 * Typical usage is when a fallback for type class `TC` is defined in its companion, like
 * {{{
 *   object TC {
 *     implicit def anyTC[T]: TC[T] = ...
 *   }
 * }}}
 *
 * With the example of `LowPriority[T]` above,
 * {{{
 *   trait TC[T] {
 *     def prop: Option[Boolean]
 *   }
 *
 *   trait LowPriTC {
 *     // default low priority TC[T] for any T, with field `prop` equal to `None`
 *     implicit def anyTC[T]: TC[T] = new TC[T] { def prop = None }
 *   }
 *
 *   object TC extends LowPriTC {
 *     // TC[Int] available by default, with field `prop` equal to `Some(true)`
 *     implicit val intTC: TC[Int] = new TC[Int] { def prop = Some(true) }
 *   }
 *
 *   // extra `TC[T]`, with field `prop` equal to `Some(false)`
 *   implicit def extraTC[T](implicit ev: LowPriority[Ignoring[Witness.`"anyTC"`.T, TC[T]]]): TC[T] =
 *     new TC[T] { def prop = Some(false) }
 *
 *   // Already available instance `intTC` is still found, because `extraTC[Int]` requires a
 *   // `LowPriority[TC[Int]]`, that will refuse to materialize (because `LowPriority` is able to
 *   // know about the already available `intTC`.)
 *   assert(implicitly[TC[Int]].prop == true)
 *
 *   // `extraTC[String]` is found, as the default `anyTC[String]` is ignored,
 *   assert(implicitly[TC[String]].prop == false)
 * }}}
 */
trait Ignoring[M, T]


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

  def ignoringTpe: Type = typeOf[Ignoring[_, _]].typeConstructor

  object IgnoringTpe {
    def unapply(tpe: Type): Option[(Type, Type)] =
      tpe.dealias match {
        case TypeRef(_, cpdTpe, List(mTpe, tTpe))
          if cpdTpe.asType.toType.typeConstructor =:= ignoringTpe =>
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
    ignoring: String
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
      def ignored = actualTree match {
        case TypeApply(method, other) => method.toString().endsWith(ignoring)
        case _ => false
      }

      ignoring.isEmpty || !ignored
    }

    if (existingInstAvailable)
      c.abort(c.enclosingPosition, s"$innerTpe available elsewhere")
    else {
      val innerTpe0 =
        if (ignoring.isEmpty)
          innerTpe
        else
          appliedType(ignoringTpe, List(internal.constantType(Constant(ignoring)), innerTpe))

      val low = q"""
        new _root_.shapeless.LowPriority[$innerTpe0] {} : _root_.shapeless.LowPriority[$innerTpe0]
      """
      val lowTpe = appliedType(lowPriorityTpe, List(innerTpe0))

      state.closeInst(wrappedTpe, low, lowTpe)
    }
  }

  private def withIgnored(tpe: Type): Either[String, (Type, String)] =
    tpe match {
      case IgnoringTpe(mTpe, innerTpe0) =>
        mTpe match {
          case ConstantType(Constant(ignored: String)) if ignored.nonEmpty => Right((innerTpe0, ignored))
          case _ => Left(s"Unsupported ignored type: $mTpe")
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
            tpeIgnored <- withIgnored(innerTpe).right
          } yield {
            val (tpe, ignored) = tpeIgnored
            deriveLowPriority(state, extState, update)(instTpe, tpe, ignored)
          }

        Some(res)

      case _ => None
    }
}


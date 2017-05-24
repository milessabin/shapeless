package shapeless

import scala.language.experimental.macros
import scala.reflect.macros.whitebox

trait Derived[+T] extends Serializable {
  def value: T
}

object Derived extends LazyExtensionCompanion {
  def apply[T](t: T): Derived[T] =
    new Derived[T] {
      def value = t
    }

  implicit def init[T]: Derived[T] = macro initImpl

  def instantiate(ctx0: DerivationContext): LazyExtension { type Ctx = ctx0.type } =
    new DerivedLazyExtension {
      type Ctx = ctx0.type
      val ctx: ctx0.type = ctx0
    }
}

trait DerivedTypes {
  type C <: whitebox.Context
  val c: C

  import c.universe._

  def deriveTpe: Type = typeOf[Derived[_]].typeConstructor

  object DeriveTpe {
    def unapply(tpe: Type): Option[(Type, Type)] =
      tpe.dealias match {
        case TypeRef(_, cpdTpe, List(derivedTpe))
          if cpdTpe.asType.toType.typeConstructor =:= deriveTpe =>
          derivedTpe.dealias match {
            case TypeRef(_, tcTpe, List(dTpe)) => Some((tcTpe.asType.toType.typeConstructor, dTpe))
            case _ => None
          }

        case _ => None
      }
  }

}

trait DerivedLazyExtension extends DerivedTypes with LazyExtension with CaseClassMacros {
  type C = ctx.c.type
  lazy val c: C = ctx.c

  import ctx._
  import c.universe._

  case object ThisState
  type ThisState = ThisState.type

  def id = "derive"

  def initialState = ThisState


  def isProductIfGeneric(tpe: Type) =
    if (isProduct(tpe)) Right(true)
    else if (isCoproduct(tpe)) Right(false)
    else Left(s"$tpe is not case class like or the root of a sealed family of types")

  lazy val typeClassTpe = typeOf[TypeClass[Any]].typeConstructor
  lazy val labelledTypeClassTpe = typeOf[LabelledTypeClass[Any]].typeConstructor
  lazy val productTypeClassTpe = typeOf[ProductTypeClass[Any]].typeConstructor
  lazy val labelledProductTypeClassTpe = typeOf[LabelledProductTypeClass[Any]].typeConstructor

  lazy val genericTpe = typeOf[Generic[_]].typeConstructor

  def typeClass(isProduct: Boolean, tpe: Type, state: State): Either[String, (State, Ident, Boolean)] =
    ctx.derive(state)(appliedType(if (isProduct) productTypeClassTpe else typeClassTpe, tpe)).convert match {
      case Right((state0, i)) => Right((state0, i.ident, false))
      case Left(msg) =>
        ctx.derive(state)(appliedType(if (isProduct) labelledProductTypeClassTpe else labelledTypeClassTpe, tpe)).convert match {
          case Right((state1, i)) => Right((state1, i.ident, true))
          case Left(msg0) => Left(s"$msg, $msg0")
        }
    }

  // Badly needed this in doDerive below
  sealed trait Either[+L, +R] extends Product with Serializable {
    def filter(p: R => Boolean) = this match {
      case Left(_) => this
      case Right(r) => assert(p(r)); this
    }
    def withFilter(p: R => Boolean) = filter(p)
    def map[U](f: R => U) = this match {
      case Left(l) => Left(l)
      case Right(r) => Right(f(r))
    }
    def flatMap[LL >: L, U](f: R => Either[LL, U]) = this match {
      case Left(l) => Left(l)
      case Right(r) => f(r)
    }

    def toScala: scala.Either[L, R] = this match {
      case Left(l) => scala.Left(l)
      case Right(r) => scala.Right(r)
    }
  }
  case class Left[L](l: L) extends Either[L, Nothing]
  case class Right[R](r: R) extends Either[Nothing, R]

  implicit def fromScalaEither[L, R](e: scala.Either[L, R]): Either[L, R] =
    e.fold(Left(_), Right(_))

  implicit class ScalaEitherOps[L, R](e: scala.Either[L, R]) {
    def convert: Either[L, R] = fromScalaEither(e)
  }

  def doDerive(baseTpe: Type, tcTpe: Type, dTpe: Type, state: State) =
    for {
      isProduct1 <- isProductIfGeneric(dTpe)
      (state1, tc, isLabelled) <- typeClass(isProduct1, tcTpe, state)
      (state2, genInst) <- ctx.derive(state1)(appliedType(genericTpe, dTpe))

      labelTpes =
        if (isProduct1) {
          if (dTpe =:= typeOf[Unit] || isCaseObjectLike(dTpe.typeSymbol.asClass))
            Nil
          else
            fieldsOf(dTpe).map { case (name, tpe0) => nameAsString(name) -> devarargify(tpe0) }
        } else
          ctorsOf(dTpe).map { tpe0 => nameAsString(nameOf(tpe0)) -> tpe0 }

      pointMethod = TermName(if (isProduct1) "emptyProduct" else "emptyCoproduct")
      prodMethod = TermName(if (isProduct1) "product" else "coproduct")

      (t, state3) <- labelTpes.foldRight(Right((q"$tc.$pointMethod", state2)): Either[String, (Tree, State)]) {
        case ((lab, tpe), acc) =>
          acc.flatMap { case (t, s) =>
            ctx.derive(s)(appliedType(tcTpe, tpe)).convert.map { case (s0, inst) =>
              val args = (
                if (isLabelled) Seq(q"$lab")
                else Nil
              ) ++ Seq(
                inst.ident,
                t
              )

              (q"$tc.$prodMethod(..$args)", s0)
            }
          }
      }
    } yield {
      val gen = genInst.ident
      state3.closeInst(baseTpe, q"_root_.shapeless.Derived($tc.project($t, $gen.to, $gen.from))", baseTpe)
    }

  def derive(
    state: State,
    extState: ThisState,
    update: (State, ThisState) => State )(
    tpe: Type
  ): Option[scala.Either[String, (State, Instance)]] =
    tpe match {
      case DeriveTpe(tcTpe, dTpe) =>
        Some(state.lookup(tpe).convert match {
          case Left(s) => doDerive(tpe, tcTpe, dTpe, s).toScala
          case Right((s, i)) => scala.Right((s, i))
        })
      case _ =>
        None
    }

}

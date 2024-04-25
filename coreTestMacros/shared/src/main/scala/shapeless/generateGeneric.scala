package shapeless

import scala.annotation.{StaticAnnotation, compileTimeOnly}
import scala.reflect.macros.blackbox
import scala.language.experimental.macros

@compileTimeOnly("enable macro annotations")
class generateGeneric extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro GenerateGenericMacroImpl.macroTransformImpl
}

object GenerateGenericMacroImpl {
  def macroTransformImpl(c: blackbox.Context)(annottees: c.Tree*): c.Tree = {
    import c.universe._

    def modifyObject(obj: Tree): Tree = obj match {
      case q"$mods object $tname extends { ..$earlydefns } with ..$parents { $self => ..$body }" =>
        q"""$mods object $tname extends { ..$earlydefns } with ..$parents { $self =>
          ..$body
          _root_.shapeless.Generic[${tname.toTypeName}](_root_.shapeless.Generic.materialize)
        }"""
      case _ => sys.error("impossible")
    }

    def modify(cls: Tree, obj: Tree): Tree = q"..${Seq(cls, modifyObject(obj))}"

    annottees match {
      case (cls: ClassDef) :: (obj: ModuleDef) :: Nil => modify(cls, obj)
      case (cls: ClassDef) :: Nil => modify(cls, q"object ${cls.name.toTermName}")
      // this works for the companion object of a sealed trait or top-level case class but not nested case class
      case (obj: ModuleDef) :: Nil => modifyObject(obj)
      case _ => c.abort(c.enclosingPosition, "@generateGeneric can annotate only traits, classes, and objects")
    }
  }
}
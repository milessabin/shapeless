/*
 * Copyright (c) 2012-13 Miles Sabin 
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

import scala.language.experimental.macros

import scala.collection.breakOut
import scala.reflect.macros.Context

trait Generic[T] { self =>
  type Repr
  def to(t : T) : Repr
  def from(r : Repr) : T
}

object Generic {
  implicit def apply[T] = macro GenericMacros.materialize[T]
  
  def identity[T] = new Generic[T] {
    type Repr = T
    def to(t : T) : T = t 
    def from(r : T) : T = r
  }
}

trait GenericAux[T, Repr] { self =>
  def to(t : T) : Repr
  def from(r : Repr) : T
}

object GenericAux {
  def apply[T, U](implicit gen: GenericAux[T, U]) = gen
  
  implicit def materialize[T](implicit gen: Generic[T]): GenericAux[T, gen.Repr] = new GenericAux[T, gen.Repr] {
    def to(t : T) : gen.Repr = gen.to(t)
    def from(r : gen.Repr) : T = gen.from(r)
  }
}

object GenericMacros {
  def materialize[T: c.WeakTypeTag](c : Context): c.Expr[Generic[T]] = {
    import c.universe._
    import definitions._
    import Flag._

    val genericSym = c.mirror.staticClass("shapeless.Generic")
    val hlistSym = c.mirror.staticClass("shapeless.HList")
    val hlistTpe = hlistSym.asClass.toType
    
    val shapelessNme = TermName("shapeless")
    val inlSel = Select(Ident(shapelessNme), TermName("Inl"))
    val inrSel = Select(Ident(shapelessNme), TermName("Inr"))
      
    val pendingSuperCall = Apply(Select(Super(This(tpnme.EMPTY), tpnme.EMPTY), nme.CONSTRUCTOR), List())
  
    def mkProductIso(className: TypeName, tpe: Type, sym: ClassSymbol): ClassDef = {
      val fields = tpe.declarations.toList.collect {
        case x: TermSymbol if x.isVal && x.isCaseAccessor => x
      }
  
      val HNilTypeTree   = Select(Ident(TermName("shapeless")), TypeName("HNil"))
      val HNilValueTree  = Select(Ident(TermName("shapeless")), TermName("HNil"))
  
      val HConsTypeTree  = Select(Ident(TermName("shapeless")), TypeName("$colon$colon"))
      val HConsValueTree = Select(Ident(TermName("shapeless")), TermName("$colon$colon"))
  
      def mkHListType: Tree = {
        fields.map { f => TypeTree(f.typeSignatureIn(tpe)) }.foldRight(HNilTypeTree : Tree) {
          case (t, acc) => AppliedTypeTree(HConsTypeTree, List(t, acc))
        }
      }
  
      def mkHListValue: Tree = {
        fields.map(_.name.toString.trim).foldRight(HNilValueTree : Tree) {
          case (v, acc) => Apply(HConsValueTree, List(Select(Ident(TermName("t")), TermName(v)), acc))
        }
      }
  
      def mkNth(n: Int): Tree =
        Select(
          (0 until n).foldRight(Ident(TermName("r")) : Tree) {
            case (_, acc) => Select(acc, TermName("tail"))
          },
          TermName("head")
        )
  
      def mkCaseClassValue: Tree =
        Apply(
          Select(Ident(sym.companionSymbol), TermName("apply")),
          (0 until fields.length).map(mkNth(_)).toList
        )
  
      ClassDef(Modifiers(FINAL), className, List(),
        Template(
          List(AppliedTypeTree(Ident(genericSym), List(TypeTree(tpe)))),
          emptyValDef,
          List(
            DefDef(
              Modifiers(), nme.CONSTRUCTOR, List(),
              List(List()),
              TypeTree(),
              Block(List(pendingSuperCall), Literal(Constant(())))),
              
            TypeDef(
              Modifiers(), TypeName("Repr"), List(),
              mkHListType),
              
            DefDef(
              Modifiers(), TermName("to"), List(),
              List(List(ValDef(Modifiers(PARAM), TermName("t"), TypeTree(tpe), EmptyTree))),
              TypeTree(),
              mkHListValue),

            DefDef(
              Modifiers(), TermName("from"), List(),
              List(List(ValDef(Modifiers(PARAM), TermName("r"), mkHListType, EmptyTree))),
              TypeTree(),
              mkCaseClassValue)
          )
        )
      )
    }
    
    def mkCoproductIso(className: TypeName, base: TypeRef, sym: ClassSymbol): ClassDef = {
      def normalize(elem: Symbol): Option[TypeTree] = {
        val elemTpe = elem.asType.toType
        elem.asClass.typeParams match {
          case Nil => if (elemTpe <:< base) Some(TypeTree(elemTpe)) else None
          case tpes =>
            val appliedTpe = appliedType(elemTpe, base.args) 
            if (appliedTpe <:< base) Some(TypeTree(appliedTpe)) else None
        }
      }

      val elems = sym.knownDirectSubclasses.toList.flatMap(normalize(_))
      
      val CoproductTypeTree  = Select(Ident(TermName("shapeless")), TypeName("$colon$plus$colon"))
  
      def mkCoproductType: Tree =
        elems.reduceRight(
          (a: TypeTree, b: Tree) => AppliedTypeTree(CoproductTypeTree, List(a, b))
        )
      
      def mkCoproductValue(i: Int): Tree = {
        val last = i == elems.length-1
        def loop(j: Int): Tree =
          if(j == 0) {
            if(last)
              Ident(TermName("x"))
            else
              Apply(
                Select(inlSel, TermName("apply")),
                List(Ident(TermName("x")))
              )
          } else {
            Apply(
              Select(inrSel, TermName("apply")),
              List(loop(j-1))
            )
          }
        loop(i)
      }
      
      def mkToCase(i: Int): CaseDef = 
        CaseDef(Bind(TermName("x"), Typed(Ident(nme.WILDCARD), elems(i))), EmptyTree, mkCoproductValue(i))
      
      def mkCoproductPattern(i: Int): Tree = {
        val last = i == elems.length-1
        def loop(j: Int): Tree =
          if(j == 0) {
            if(last)
              Bind(TermName("x"), Ident(nme.WILDCARD))
            else
              Apply(inlSel,
                List(
                  Bind(TermName("x"), Ident(nme.WILDCARD))))
          } else {
            Apply(inrSel, List(loop(j-1)))
          }
        loop(i)
      }

      def mkFromCase(i: Int): CaseDef =
        CaseDef(mkCoproductPattern(i), EmptyTree, Ident(TermName("x")))
      
      ClassDef(Modifiers(FINAL), className, List(),
        Template(
          List(AppliedTypeTree(Ident(genericSym), List(TypeTree(base)))),
          emptyValDef,
          List(
            DefDef(Modifiers(), nme.CONSTRUCTOR, List(),
              List(List()),
              TypeTree(),
              Block(List(pendingSuperCall), Literal(Constant(())))),
              
            TypeDef(
              Modifiers(), TypeName("Repr"), List(),
              mkCoproductType),
              
            DefDef(Modifiers(), TermName("to"), List(),
              List(List(ValDef(Modifiers(PARAM), TermName("t"), TypeTree(base), EmptyTree))),
              mkCoproductType,
              Match(
                Ident(TermName("t")),
                (0 until elems.length).map(mkToCase(_))(breakOut)
              )
            ),
            
            DefDef(Modifiers(), TermName("from"), List(),
              List(List(ValDef(Modifiers(PARAM), TermName("r"), mkCoproductType, EmptyTree))),
              TypeTree(base),
              Match(
                Ident(TermName("r")),
                (0 until elems.length).map(mkFromCase(_))(breakOut)
              )
            )
          )
        )
      )
    }

    val tpe = c.weakTypeOf[T]
    if(tpe <:< hlistTpe)
      reify { Generic.identity[T] }
    else {
      val sym = tpe.typeSymbol
      
      def badType() = 
        c.abort(c.enclosingPosition, s"$sym is not a case class or a sealed trait or abstract class")
  
      if (!sym.isClass) badType()
      val classSym = sym.asClass 
        
      val className = TypeName(c.freshName)
      
      val isoClass =
        if (classSym.isCaseClass)
          mkProductIso(className, tpe, classSym)
        else if (classSym.isSealed)
          tpe match {
            case base: TypeRef => mkCoproductIso(className, base, classSym)
            case _ => badType()
          }
        else
          badType()
        
      c.Expr[Generic[T]](
        Block(
          List(isoClass),
          Apply(Select(New(Ident(className)), nme.CONSTRUCTOR), List())
        )
      )
    }
  }
}

// vim: expandtab:ts=2:sw=2

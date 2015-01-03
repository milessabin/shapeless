package shapeless
package ops

import shapeless.labelled.FieldType

object prettystring {  
  trait PrettyString[T] {
    def suggestedName: String

    /**
     * Prints the elements of `t` in `b`
     * 
     * @param sepBefore: if Left, a separator between elements, if Right, a string to prepend before each element  
     */
    def addElements(t: T, b: StringBuilder, sepBefore: Either[String, String]): Unit

    def mkString(t: T, prefix: String, sep: String, suffix: String): String = {
      val b = new StringBuilder      
      b ++= prefix; addElements(t, b, Left(sep)); b ++= suffix      
      b.result()
    }
    
    def apply(t: T): String =
      mkString(t, s"$suggestedName(", ", ", ")")
  }
  
  trait LowPriorityPrettyString {
    implicit def hconsNonHeadRecursivePrettyString[K <: Symbol, H, T <: HList](implicit
      key: Witness.Aux[K],
      tailPrettyPrint: Lazy[PrettyString[T]]
    ): PrettyString[FieldType[K, H] :: T] =
      new PrettyString[FieldType[K, H] :: T] {
        def suggestedName = tailPrettyPrint.value.suggestedName
        def addElements(r: FieldType[K, H] :: T, b: StringBuilder, sepBefore: Either[String, String]) = {
          sepBefore match {
            case Right(before) =>
              b ++= before
            case _ =>
          }
          
          b ++= key.value.name
          b += '='
          b ++= r.head.toString

          tailPrettyPrint.value.addElements(r.tail, b, Right(sepBefore.merge))
        }
      }

    implicit def cconsNonHeadRecursivePrettyString[K <: Symbol, H, T <: Coproduct](implicit
      key: Witness.Aux[K],
      tailPrettyPrint: Lazy[PrettyString[T]]
    ): PrettyString[FieldType[K, H] :+: T] =
      new PrettyString[FieldType[K, H] :+: T] {
        def suggestedName = tailPrettyPrint.value.suggestedName
        def addElements(u: FieldType[K, H] :+: T, b: StringBuilder, sepBefore: Either[String, String]) =
          u match {
            case Inl(h) =>
              sepBefore match {
                case Right(before) =>
                  b ++= before
                case _ =>
              }

              b ++= key.value.name
              b += '='
              b ++= h.toString
              
            case Inr(t) =>
              tailPrettyPrint.value.addElements(t, b, sepBefore)
          }
      }
  }
  
  object PrettyString extends LowPriorityPrettyString {
    def apply[T](implicit prettyString: PrettyString[T]): PrettyString[T] = prettyString
    
    implicit def hnilPrettyString[R <: HNil]: PrettyString[R] =
      new PrettyString[R] {
        val suggestedName = "Record"
        def addElements(r: R, b: StringBuilder, sepBefore: Either[String, String]) = {}
      }
    
    implicit def hconsHeadRecursivePrettyString[K <: Symbol, H, T <: HList](implicit
      key: Witness.Aux[K],
      headPrettyPrint: Lazy[PrettyString[H]],
      tailPrettyPrint: Lazy[PrettyString[T]]                                                        
    ): PrettyString[FieldType[K, H] :: T] =
      new PrettyString[FieldType[K, H] :: T] {
        def suggestedName = tailPrettyPrint.value.suggestedName
        def addElements(r: FieldType[K, H] :: T, b: StringBuilder, sepBefore: Either[String, String]) = {
          sepBefore match {
            case Right(before) =>
              b ++= before
            case _ =>
          }

          b ++= key.value.name
          b += '='
          b ++= headPrettyPrint.value(r.head)
          
          tailPrettyPrint.value.addElements(r.tail, b, Right(sepBefore.merge))
        }
      }

    implicit val cnilPrettyString: PrettyString[CNil] =
      new PrettyString[CNil] {
        val suggestedName = "Union"
        def addElements(u: CNil, b: StringBuilder, sepBefore: Either[String, String]) = {}
      }

    implicit def cconsHeadRecursivePrettyString[K <: Symbol, H, T <: Coproduct](implicit
      key: Witness.Aux[K],
      headPrettyPrint: Lazy[PrettyString[H]],
      tailPrettyPrint: Lazy[PrettyString[T]]
    ): PrettyString[FieldType[K, H] :+: T] =
      new PrettyString[FieldType[K, H] :+: T] {
        def suggestedName = tailPrettyPrint.value.suggestedName
        def addElements(u: FieldType[K, H] :+: T, b: StringBuilder, sepBefore: Either[String, String]) = 
          u match {
            case Inl(h) =>
              sepBefore match {
                case Right(before) =>
                  b ++= before
                case _ =>
              }

              b ++= key.value.name
              b += '='
              b ++= headPrettyPrint.value(h)
              
            case Inr(t) =>
              tailPrettyPrint.value.addElements(t, b, sepBefore)
          }
      }
    
    implicit def instancePrettyString[F, G](implicit
      manifest: Manifest[F],
      lgen: LabelledGeneric.Aux[F, G],
      instance: PrettyString[G]                                     
    ): PrettyString[F] =
      new PrettyString[F] {
        val suggestedName = {
          val s = manifest.toString().takeWhile(_ != '[')
          s.substring((s.lastIndexOf('.') max s.lastIndexOf('$')) + 1)
        }
        def addElements(f: F, b: StringBuilder, sepBefore: Either[String, String]) =
          instance.addElements(lgen.to(f), b, sepBefore)
      }
  }  
}

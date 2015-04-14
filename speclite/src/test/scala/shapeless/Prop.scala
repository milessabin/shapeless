// Based on work from Rickard Nilsson at http://www.scalacheck.org
package shapeless.test

class Properties(val name: String) extends Prop {

  private val props = new scala.collection.mutable.ListBuffer[(String,Prop)]

  /** Returns one property which holds if and only if all of the
    *  properties in this property collection hold */
  private def oneProperty: Prop = Prop.all((properties map (_._2)):_*)

  /** Returns all properties of this collection in a list of name/property
    *  pairs.  */
  def properties: Seq[(String,Prop)] = props

  def apply(p: Gen.Parameters) = oneProperty(p)

  class PropertySpecifier() {
    def update(propName: String, p: Prop) = props += ((name+"."+propName, p))
  }

  lazy val property = new PropertySpecifier()
}

/** Helper class to satisfy ScalaJS compilation. Do not use this directly,
  *  use [[Prop.apply]] instead. */
class PropFromFun(f: Gen.Parameters => Prop.Result) extends Prop {
  def apply(prms: Gen.Parameters) = f(prms)
}

@scala.scalajs.js.annotation.JSExportDescendentClasses
@scala.scalajs.js.annotation.JSExportDescendentObjects
trait Prop {

  import Prop.{Result, secure}
  import Gen.Parameters

  def apply(prms: Parameters): Result

  def map(f: Result => Result): Prop = Prop(prms => f(this(prms)))

  def flatMap(f: Result => Prop): Prop = Prop(prms => f(this(prms))(prms))

  def combine(p: Prop)(f: (Result, Result) => Result) =
    for(r1 <- this; r2 <- p) yield f(r1,r2)

  def &&(p: => Prop) = combine(secure(p))(_ && _)

  override def toString = "Prop"
}

object Prop {
   import Gen.Parameters

   /** A property argument */
   case class Arg[+T](label: String, arg: T, shrinks: Int, origArg: T,
       prettyArg: Pretty, prettyOrigArg: Pretty
   )

   /** The result of evaluating a property */
   case class Result(status: Status, args: List[Arg[Any]] = Nil,
       collected: Set[Any] = Set.empty, labels: Set[String] = Set.empty) {

     def &&(r: Result) = (this.status, r.status) match {
       case (Exception(_),_) => this
       case (_,Exception(_)) => r

       case (False,_) => this
       case (_,False) => r

       case (Undecided,_) => this
       case (_,Undecided) => r

       case (_,Proof) => mergeRes(this, r, this.status)
       case (Proof,_) => mergeRes(this, r, r.status)

       case (True,True) => mergeRes(this, r, True)
     }
   }

   sealed trait Status
   case object Proof extends Status
   case object True extends Status
   case object False extends Status
   case object Undecided extends Status

   /** Evaluating the property raised an exception */
   sealed case class Exception(e: Throwable) extends Status {
     override def equals(o: Any) = o match {
       case Exception(_) => true
       case _ => false
     }
   }

   def apply(f: Parameters => Result): Prop = new PropFromFun(prms =>
     try f(prms) catch {
       case e: Throwable => Result(status = Exception(e))
     }
   )

   def apply(r: Result): Prop = Prop.apply(prms => r)

   def apply(b: Boolean): Prop = if(b) proved else falsified

   def all(ps: Prop*) =
     if(ps.isEmpty) proved
     else Prop(prms => ps.map(p => p(prms)).reduceLeft(_ && _)
   )
   def exception(e: Throwable): Prop = Prop(Result(status = Exception(e)))

   def secure[P <% Prop](p: => P): Prop =
     try (p: Prop) catch { case e: Throwable => exception(e) }

   implicit def propBoolean(b: Boolean): Prop = Prop(b)

   lazy val undecided = Prop(Result(status = Undecided))

   lazy val falsified = Prop(Result(status = False))

   lazy val proved = Prop(Result(status = Proof))

   lazy val passed = Prop(Result(status = True))

   lazy val exception: Prop = exception(null)

   private[test] def mergeRes(x: Result, y: Result, st: Status) =
     Result(status = st, args = x.args ++ y.args,
       collected = x.collected ++ y.collected, labels = x.labels ++ y.labels
     )
 }

object Gen {

  trait Parameters {

    val size: Int

    def withSize(size: Int): Parameters = cp(size = size)

    private case class cp(size: Int = size) extends Parameters

  }

  object Parameters {
    trait Default extends Parameters {
      val size: Int = 100
    }

    val default: Parameters = new Default {}
  }
}
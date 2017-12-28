package shapeless.examples

import shapeless._
import shapeless.poly._
import record._
import shapeless.ops.record.DeepMerger
import shapeless.ops.record.Extractor
import shapeless.ops.record.MapValues
/**
  * Depth subtyping of extensible records
  * http://stackoverflow.com/questions/32803930/checking-for-subtype-relationship-between-extensible-records-in-shapeless/35702275#35702275
  *
  * @author Ievgen Garkusha
  */
object recordsubtyping extends App{

  class City(val name: String){
    //Dirty hack for demo purpose. In real case the Eq-like typeclass would be needed in order to perform correct comparison based on type.
    override def equals(v: Any) = v.asInstanceOf[City].name == name
  }
  class PopulatedCity(override val name: String, val population: Long) extends City(name)

  val employeeId = Record(firstName = "Jane", lastName = "Doe", title = "software engineer")

  val employee1 = Record(id = employeeId, city = new PopulatedCity("San Francisco", 2), company =  "Foo Inc.")
  val employee2 = employee1.updated('company, "Bar Inc.")
  val employee3 = employee1.updated('city, new PopulatedCity("Chernobyl", 1) )

  type PersonId = Record.`'firstName -> String, 'lastName -> String`.T
  type Person = Record.`'id -> PersonId, 'city -> City`.T

  val somePerson: Person = Record(id = Record(firstName = "Jane", lastName = "Doe"), city = new City("San Francisco"))


  trait default extends Poly1 {
    implicit def id[T] = at[T](identity)
  }
  object toUpper extends default {
    implicit def toUpStr = at[String](_.toUpperCase)
    implicit def toUpCity = at[City](c => new City(c.name.toUpperCase))
    implicit def toUpHl[L <: HList](implicit mv: MapValues[this.type, L]) = at[L](mv(_))
  }

  //isSamePerson
  println(employee1.extract[Person] == somePerson)//true
  println(employee1.extract[Person] == employee2.extract[Person])//true
  println(employee1.extract[Person] == employee3.extract[Person])//false

  //simple inheritance-like behavior
  val scorePerson: Person => Double = _ => 42D
  val transformPerson: Person => Person = _.mapValues(toUpper)

  val transformedPerson = Record(id = Record(firstName = "JANE", lastName = "DOE"), city = new City("SAN FRANCISCO"))

  println(transformPerson(employee1.extract[Person]) == transformedPerson)//true

  println(scorePerson(employee1.extract[Person]) == 42)//true


  //transform Person structure preserving Employee shape
  //this is only possible if no nominal subtyping relation is present between the record fields ('city' in this case)
  val noNominalSubtypingEmployee = employee1.updateWith('city)(c => c: City)

  def transform[L <: HList, X <: HList, Y <: HList, O <: HList](input: L)(transform: X => Y)
    (implicit
      extract: Extractor[L, X],
      merge: DeepMerger.Aux[L, Y, O],
      ev: O =:= L
    ): L = merge(input, transform(extract(input)))


  val transformedEmployee = Record(
      id = Record(firstName = "JANE", lastName = "DOE", title = "software engineer"),
      city = new City("SAN FRANCISCO"),
      company =  "Foo Inc."
  )

  println(transform(noNominalSubtypingEmployee)(transformPerson) == transformedEmployee) //true

}

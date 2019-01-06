package org.jetbrains.plugins.scala.lang.macros

import org.jetbrains.plugins.scala.debugger.Scala_2_12

class ShapelessLensesTest extends ShapelessConformanceTestBase()(Scala_2_12) {
  override protected def commonHeader: String =
    """
      |import shapeless._
      |case class Address(street : String, city : String, postcode : String)
      |case class Person(name : String, age : Int, address : Address)
      |
      |// Some lenses over Person/Address ...
      |val nameLens     = lens[Person].name
      |val ageLens      = lens[Person].age
      |val addressLens  = lens[Person].address
      |val streetLens   = lens[Person].address.street
      |val cityLens     = lens[Person].address.city
      |val postcodeLens = lens[Person].address.postcode
      |
      |val person = Person("Joe Grey", 37, Address("Southover Street", "Brighton", "BN2 9UA"))
      |
    """.stripMargin

  def testLens(): Unit = {
    val snippets = Seq(
      "val age1: Int = ageLens.get(person)",
      "val person2: Person = ageLens.set(person)(38)",
      "val person3: Person = ageLens.modify(person)(_ + 1)",
      "val street: String = streetLens.get(person3)",
      "val person4: Person = streetLens.set(person3)(\"Montpelier Road\")",
      """
        |val nameAgeCityLens = nameLens ~ ageLens ~ cityLens
        |val nac1: (String, Int, String) = nameAgeCityLens.get(person)
      """.stripMargin,
      """
        |val nameAgeCityLens = nameLens ~ ageLens ~ cityLens
        |val person5: Person = nameAgeCityLens.set(person)(\"Joe Soap\", 27, \"London\")
      """.stripMargin
    )
    snippets.foreach(runTest(_, testEquiv = true))
  }
}

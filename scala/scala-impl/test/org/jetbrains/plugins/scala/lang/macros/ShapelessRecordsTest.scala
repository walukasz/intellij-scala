package org.jetbrains.plugins.scala.lang.macros

import org.jetbrains.plugins.scala.debugger.Scala_2_12


/**
  * https://github.com/milessabin/shapeless/blob/master/core/src/test/scala/shapeless/records.scala
  */
class ShapelessRecordsTest extends ShapelessConformanceTestBase()(Scala_2_12) {
  override protected def commonHeader: String =
    s"""
       |import shapeless._
       |import shapeless.labelled._
       |import shapeless.ops.record.LacksKey
       |import shapeless.record._
       |import shapeless.syntax.singleton._
       |import shapeless.syntax.std.maps._
       |import shapeless.test._
       |import shapeless.testutil._
       |import shapeless.ops.record.{ RemoveAll, UnzipFields }
       |
       |object intField1 extends FieldOf[Int]
       |object intField2 extends FieldOf[Int]
       |object stringField1 extends FieldOf[String]
       |object stringField2 extends FieldOf[String]
       |object boolField1 extends FieldOf[Boolean]
       |object boolField2 extends FieldOf[Boolean]
       |object doubleField1 extends FieldOf[Double]
       |object doubleField2 extends FieldOf[Double]
     """.stripMargin

  def testGet(): Unit = {
    val header =
      """
        |val r1 =
        |  (intField1    ->>    23) ::
        |  (stringField1 ->> "foo") ::
        |  (boolField1   ->>  true) ::
        |  (doubleField1 ->>   2.0) ::
        |  HNil
      """.stripMargin

    val snippets = Seq(
      "val v1: Int = r1.get(intField1)",
      "val v2: String = r1.get(stringField1)",
      "val v3: Boolean = r1.get(boolField1)",
      "val v4: Double = r1.get(doubleField1)"
    )
    snippets.foreach(runTest(_, header))
  }

  def testGetLiteral(): Unit = {
    val header =
      """
        |val r1 =
        |  ("intField1"    ->>    23) ::
        |  ("stringField1" ->> "foo") ::
        |  ("boolField1"   ->>  true) ::
        |  ("doubleField1" ->>   2.0) ::
        |  HNil
      """.stripMargin

    val snippets = Seq(
      """val v1: Int = r1.get("intField1")""",
      """val v2: String = r1.get("stringField1")""",
      """val v3: Boolean = r1.get("boolField1")""",
      """val v4: Double = r1.get("doubleField1")"""
    )
    snippets.foreach(runTest(_, header))
  }

  def testFieldAt(): Unit = {
    val header =
      """
        |val r1 =
        |  (stringField1 ->>  "toto") ::
        |  (boolField1   ->>  true)   ::
        |  HNil
      """.stripMargin

    val snippets = Seq(
      "val v1: stringField1.F = r1.fieldAt(stringField1)",
      "val v2: boolField1.F = r1.fieldAt(boolField1)"
    )
    snippets.foreach(runTest(_, header))
  }

  def testAt(): Unit = {
    val header =
      """
        |val r1 =
        |  (intField1    ->>    23) ::
        |  (stringField1 ->> "foo") ::
        |  (boolField1   ->>  true) ::
        |  (doubleField1 ->>   2.0) ::
        |  HNil
      """.stripMargin

    val snippets = Seq(
      "val v1: Int = r1.at(0)",
      "val v2: String = r1.at(1)",
      "val v3: Boolean = r1.at(2)",
      "val v4: Double = r1.at(3)"
    )
    snippets.foreach(runTest(_, header))
  }

  def testFromMap(): Unit = {
    val header =
      """
        |type T1 = Record.`'stringVal -> String, 'intVal -> Int, 'boolVal -> Boolean`.T
        |val in = Map('intVal -> 4, 'stringVal -> "Blarr", 'boolVal -> true)
        |import syntax.std.maps._
        |val rec: T1 = in.toRecord[T1].get
      """.stripMargin

    val snippets = Seq(
      "val v1: Int = rec('intVal)",
      "val v2: String = rec('stringVal)",
      "val v3: Boolean = rec('boolVal)",
    )
    snippets.foreach(runTest(_, header))
  }

  def testFromMap2(): Unit = {
    val header =
      """
        |type T = intField1.F :: stringField1.F :: boolField1.F :: doubleField1.F :: HNil
        |val in = Map(intField1 -> 4, stringField1 -> "Blarr", boolField1 -> true, doubleField1 -> 5.0)
        |import syntax.std.maps._
        |val rec: T = in.toRecord[T].get
      """.stripMargin

    val snippets = Seq(
      "val v1: Int = rec(intField1)",
      "val v2: String = rec(stringField1)",
      "val v3: Boolean = rec(boolField1)",
      "val v4: Double = rec(doubleField1)"
    )
    snippets.foreach(runTest(_, header))
  }

  def testAtLiterals(): Unit = {
    val header =
      """
        |val r1 =
        |  ("intField1"    ->>    23) ::
        |  ("stringField1" ->> "foo") ::
        |  ("boolField1"   ->>  true) ::
        |  ("doubleField1" ->>   2.0) ::
        |  HNil
      """.stripMargin

    val snippets = Seq(
      "val v1: Int = r1.at(0)",
      "val v2: String = r1.at(1)",
      "val v3: Boolean = r1.at(2)",
      "val v4: Double = r1.at(3)"
    )
    snippets.foreach(runTest(_, header))
  }

  def testUpdate(): Unit = {
    val header =
      """
        |val r1 =
        |  (intField1    ->>    23) ::
        |  (stringField1 ->> "foo") ::
        |  (boolField1   ->>  true) ::
        |  (doubleField1 ->>   2.0) ::
        |  HNil
      """.stripMargin

    val snippets = Seq(
      """
        |val r2 = r1.updated(intField1, 7)
        |val v1: Int = r2.get(intField1)
      """.stripMargin,
      """
        |val r3 = r1.updated(stringField1, "wibble")
        |val v2: String = r3.get(stringField1)
      """.stripMargin,
      """
        |val r4 = r1.updated(boolField1, false)
        |val v3: Boolean = r4.get(boolField1)
      """.stripMargin,
      """
        |val r5 = r1.updated(doubleField1, 1.0)
        |val v4: Double = r5.get(doubleField1)
      """.stripMargin,
      """
        |val r6 = HNil
        |val r7 = r6.updated(boolField2, false)
        |val v5: Boolean = r7.get(boolField2)
        |
      """.stripMargin
    )
    snippets.foreach(runTest(_, header))
  }

  def testUpdateLiteral(): Unit = {
    val header =
      """
        |val r1 =
        |  ("intField1"    ->>    23) ::
        |  ("stringField1" ->> "foo") ::
        |  ("boolField1"   ->>  true) ::
        |  ("doubleField1" ->>   2.0) ::
        |  HNil
      """.stripMargin

    val snippets = Seq(
      """
        |val r2 = r1.updated("intField1", 7)
        |val v1: Int = r2.get("intField1")
      """.stripMargin,
      """
        |val r3 = r1.updated("stringField1", "wibble")
        |val v2: String = r3.get("stringField1")
      """.stripMargin,
      """
        |val r4 = r1.updated("boolField1", false)
        |val v3: Boolean = r4.get("boolField1")
      """.stripMargin,
      """
        |val r5 = r1.updated("doubleField1", 1.0)
        |val v4: Double = r5.get("doubleField1")
      """.stripMargin,
      """
        |val r6 = HNil
        |val r7 = r6.updated("boolField2", false)
        |val v5: Boolean = r7.get("boolField2")
      """.stripMargin
    )
    snippets.foreach(runTest(_, header))
  }

  def testMerge(): Unit = {
    val header =
      """
        |val r1 = 'a ->> 23 :: 'b ->> "foo" :: 'c ->> true :: HNil
        |val r2 = 'c ->> false :: 'a ->> 13 :: HNil
      """.stripMargin

    val snippet = "val rm: Record.`'a -> Int, 'b -> String, 'c -> Boolean`.T = r1.merge(r2)"
    runTest(snippet, header)
  }

  def testDeepMerge(): Unit = {
    val header =
      """
        |val r3 = Record(d = Record(x = "X1", m = "M"), e = true, x = "X")
        |val r4 = Record(d = "D", e = false, x = 2, m = 6)
        |val r5 = Record(d = "A", d = "B", d  = "C")
        |
        |//nested
        |val inner1 = Record(d = "D", e = false)
        |val inner2 = Record(d = 3, m = 2D)
        |val outer1 = Record(d = 10, e = inner1, x = "boo")
        |val outer2 = Record(x = "foo", d = -1, e = inner2)
      """.stripMargin

    val snippets = Seq(
      "r4.deepMerge(r3)",
    )
  }

  def testExtract(): Unit = {
    val header =
      """
        |val inner1 = Record(d = 3, m = 2D, x= "X")
        |val outer1 = Record(x = "foo", d = -1, e = inner1)
        |type i = Record.`'x -> String, 'd -> Int`.T
        |type i1 = Record.`'x -> Any, 'd -> Any`.T
      """.stripMargin

    val snippets = Seq(
      "val ext1: Record.`'e -> i, 'd -> Int`.T = outer1.extract[Record.`'e -> i, 'd -> Int`.T]",
      "val ext2: Record.`'e -> i1, 'd -> Any`.T = outer1.extract[Record.`'e -> i1, 'd -> Any`.T]"
    )
    snippets.foreach(runTest(_, header))
  }

  def testMergeWith(): Unit = {
    val header =
      """
        |object mergeField extends Poly2 {
        |  implicit def xor = at[Boolean, Boolean] { _ ^ _ }
        |  implicit def toDouble = at[Int, String] { _.toDouble + _.toDouble }
        |}
      """.stripMargin

    val snippets = Seq(
      """
        |val r1 = 'c ->> true :: HNil
        |val r2 = 'c ->> false :: HNil
        |val rm: Record.`'c -> Boolean`.T = r1.mergeWith(r2)(mergeField)
      """.stripMargin,
      """
        |val r1 = 'a ->> 23 :: 'b ->> "foo" :: 'c ->> true :: HNil
        |val r2 = 'c ->> false :: 'a ->> "13" :: HNil
        |val rm: Record.`'a -> Double, 'b -> String, 'c -> Boolean`.T = r1.mergeWith(r2)(mergeField)
      """.stripMargin
    )
    snippets.foreach(runTest(_, header))
  }

  def testAppend(): Unit = {
    val header =
      """
        |val r1 =
        |  (intField1    ->>    23) ::
        |  (stringField1 ->> "foo") ::
        |  HNil
      """.stripMargin

    val snippet =
      """
        |val r2 = r1 + (boolField1 ->> true)
        |val r3: intField1.F :: stringField1.F :: boolField1.F :: doubleField1.F :: HNil = r2 + (doubleField1 ->> 2.0)
      """.stripMargin
    runTest(snippet, header)
  }

  def testAppendLiteral(): Unit = {
    val header =
      """
        |val r1 =
        |  ("i" ->>  23) ::
        |  ("s" ->> "foo") ::
        |  HNil
      """.stripMargin

    val snippet =
      """
        |val r2 = r1 + ("b" ->> true)
        |val r3: FieldType["i", Int] :: FieldType["s", String] :: FieldType["b", Boolean] :: FieldType["d", Double] :: HNil =
        |  r2 + ("d" ->> 2.0)
      """.stripMargin
    runTest(snippet, header)
  }

  def testRemove(): Unit = {
    val header =
      """
        |val r1 =
        |  (intField1    ->>    23) ::
        |  (stringField1 ->> "foo") ::
        |  (boolField1   ->>  true) ::
        |  (doubleField1 ->>   2.0) ::
        |  HNil
      """.stripMargin

    val snippets = Seq(
      "val rm1: (Int, stringField1.F :: boolField1.F :: doubleField1.F :: HNil) = r1.remove(intField1)",
      "val rm2: (String, intField1.F :: boolField1.F :: doubleField1.F :: HNil) = r1.remove(stringField1)",
      "val rm3: (Boolean, intField1.F :: stringField1.F :: doubleField1.F :: HNil) = r1.remove(boolField1)",
      "val rm4: (Double, intField1.F :: stringField1.F :: boolField1.F :: HNil) = r1.remove(doubleField1)",
      "val m1: stringField1.F :: boolField1.F :: doubleField1.F :: HNil = r1 - intField1",
      "val m2: intField1.F :: boolField1.F :: doubleField1.F :: HNil = r1 - stringField1",
      "val m3: intField1.F :: stringField1.F :: doubleField1.F :: HNil = r1 - boolField1",
      "val m4: intField1.F :: stringField1.F :: boolField1.F :: HNil = r1 - doubleField1"
    )
    snippets.foreach(runTest(_, header))
  }

  def testReplace(): Unit = {
    val header =
      """
        |type R = Record.`'a -> Int, 'b -> String`.T
        |val a = Record(a = 1, b = "2")
      """.stripMargin

    val snippet = "val r: R = a.replace('a, 2)"
    runTest(snippet, header)
  }

  def testLacksKey(): Unit = {
    val header =
      """
        |def without[R <: HList, O <: HList](k: Witness)(r: R)(f: R => O)(implicit ev: LacksKey[R, k.T]): O = f(r)
        |
        |type R1 = Record.`'a -> Int, 'b -> String, 'c -> Boolean`.T
        |type R2 = Record.`'c -> Boolean, 'a -> Int, 'b -> String`.T
        |val a = Record(a = 1, b = "2")
      """.stripMargin

    val snippets = Seq(
      "val r1: R1 = without('c)(a)(_ :+ ('c ->> true))",
      "val r1: R2 = without('c)(a)(('c ->> true) +: _)"
    )
    snippets.foreach(runTest(_, header))
  }

  def testRemoveAll(): Unit = {
    val header =
      """
        |type R = Record.`'i -> Int, 's -> String, 'c -> Char, 'j -> Int`.T
        |type L = Record.`'c -> Char, 'j -> Int`.T
        |
        |type A1 = Record.`'i -> Int, 's -> String`.T
        |type A2 = Int :: String :: HNil
        |
        |val r = 'i ->> 10 :: 's ->> "foo" :: 'c ->> 'x' :: 'j ->> 42 :: HNil
        |
        |val removeAll1 = RemoveAll[R, A1]
        |val removeAll2 = RemoveAll[R, A2]
      """.stripMargin

    val snippets = Seq(
      "val (removed1, remaining1): (A1, L) = removeAll1(r)",
      "val (removed2, remaining2): (A2, L) = removeAll2(r)",
    )
    snippets.foreach(runTest(_, header))
  }

  def testMaAppingOverRecordFields(): Unit = {
    // @FIXME: not working
  }

  def testUpdateFieldByFunction(): Unit = {
    // @TODO: `WitnessWith` macro implementation
  }

  def testRenameField(): Unit = {
    val header =
      """
        |val r = ("foo" ->> 23) :: ("bar" ->> true) :: HNil
        |val r1 = r.renameField("foo", "foobar")
      """.stripMargin

    val snippet = "val v1: Int = r1.get(\"foobar\")"
    runTest(snippet, header, testEquiv = true)
  }

  def testFieldPoly(): Unit = {
    // @FIXME: wrong implicit found
  }

  def testFieldPolyOnRecord(): Unit = {
    // @FIXME: ambig. implicits
  }

  def testFieldPolyNested(): Unit = {
    // @FIXME: ambig. implicits
  }

  def testSelectDynamic(): Unit = {
    val header =
      """
        |val r = ('foo ->> 23) :: ('bar ->> true) :: HNil
        |val d = r.record
      """.stripMargin

    val snippets = Seq(
      "val v1: Int = d.foo",
      "val v2: Boolean = d.bar"
    )
    snippets.foreach(runTest(_, header, testEquiv = true))
  }

  def testNamedArgs(): Unit = {
    val snippets = Seq(
      "val r: HNil = Record()",
      "val r2: Record.`'i -> Int, 's -> String, 'b -> Boolean`.T = Record(i = 23, s = \"foo\", b = true)"
    )
    snippets.foreach(runTest(_))
  }

  def testRecordArgs(): Unit = {
    // @TODO: `RecordArgs.applyDynamic` macro
  }

  def testFromRecordArgs(): Unit = {
    // @TODO: `RecordArgs.applyDynamic` macro
  }
}

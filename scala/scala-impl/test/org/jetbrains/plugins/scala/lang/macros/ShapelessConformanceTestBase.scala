package org.jetbrains.plugins.scala.lang.macros

import org.jetbrains.plugins.scala.DependencyManagerBase.RichStr
import org.jetbrains.plugins.scala.base.libraryLoaders.{IvyManagedLoader, LibraryLoader}
import org.jetbrains.plugins.scala.debugger.ScalaVersion
import org.jetbrains.plugins.scala.lang.typeConformance.TypeConformanceTestBase

class ShapelessConformanceTestBase()(override implicit val version: ScalaVersion) extends TypeConformanceTestBase {
  override protected def additionalLibraries(): Seq[LibraryLoader] =
    IvyManagedLoader("com.chuusai" %% "shapeless" % "2.3.3") :: Nil

  protected def commonHeader: String = ""

  def runTest(snippet: String, header: String = "", testEquiv: Boolean = false): Unit =
    doTest(s"$commonHeader\n$header\n$snippet\n//True", checkEquivalence = testEquiv)
}

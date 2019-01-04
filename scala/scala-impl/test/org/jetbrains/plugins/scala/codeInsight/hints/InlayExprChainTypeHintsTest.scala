package org.jetbrains.plugins.scala.codeInsight.hints

import com.intellij.codeInsight.daemon.impl.HintRenderer
import com.intellij.openapi.editor.Inlay
import com.intellij.openapi.util.Setter
import org.jetbrains.plugins.scala.codeInsight.ScalaCodeInsightSettings

class InlayExprChainTypeHintsTest extends InlayHintsTestBase {

  import InlayHintsTestBase.{HintEnd => E, HintStart => S}
  import ScalaCodeInsightSettings.{getInstance => settings}

  def testChain(): Unit = doTest(
    s"""
       |List(1, 2, 3)$S: List[Int]$E
       |  .toSeq$S: Seq[Int]$E
       |  .filter(_ > 2)
       |  .toSet$S: Set[Int]$E
       |  .toString$S: String$E
     """.stripMargin
  )

  def testChainInValDef(): Unit = doTest(
    s"""
       |val x = List(1, 2, 3)$S: List[Int]$E
       |  .toSeq$S: Seq[Int]$E
       |  .filter(_ > 2)
       |  .toSet$S: Set[Int]$E
       |  .toString$S: String$E
     """.stripMargin
  )

  def testNoHintsWhenTurnedOf(): Unit = doTest(
    s"""
       |List(1, 2, 3)
       |  .toSeq
       |  .filter(_ > 2)
       |  .toSet
       |  .toString
     """.stripMargin,
    options = settings.showExpressionChainTypeSetter() -> false
  )

  def testBoringChainHasNoHints(): Unit = doTest(
    s"""
      |List(1, 2, 3)
      |  .filter(_ > 2)
      |  .filter(_ == 39)
      |  .map(_ + 3)
      |  .filter(_ < 2)
    """.stripMargin
  )

  def testChainWithoutLineBreaksHasNoHints(): Unit = doTest(
    s"""
       |List(1, 2, 3).toSeq.filter(_ > 2)
       |  .toSet.toString
     """.stripMargin
  )

  private def doTest(text: String, options: (Setter[java.lang.Boolean], Boolean)*): Unit = {
    def setOptions(reset: Boolean): Unit = options.foreach { case (opt, value) => opt.set(if(reset) !value else value) }

    try {
      setOptions(false)

      configureFromFileText(text)
      getFixture.testInlays(
        inlayText(_).get,
        inlayText(_).isDefined
      )
    } finally {
      setOptions(true)
    }
  }

  private val inlayText = (_: Inlay[_]).getRenderer match {
    case renderer: HintRenderer => Some(renderer.getText)
    case _ => None
  }

}

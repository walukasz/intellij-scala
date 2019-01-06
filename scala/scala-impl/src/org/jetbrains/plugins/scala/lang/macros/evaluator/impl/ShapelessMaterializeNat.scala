package org.jetbrains.plugins.scala.lang.macros.evaluator.impl

import com.intellij.psi.PsiElement
import org.jetbrains.plugins.scala.lang.macros.evaluator.{MacroContext, MacroImpl, ScalaMacroTypeable}
import org.jetbrains.plugins.scala.lang.psi.api.base.ScIntLiteral
import org.jetbrains.plugins.scala.lang.psi.api.statements.ScFunction
import org.jetbrains.plugins.scala.lang.psi.impl.ScalaPsiElementFactory
import org.jetbrains.plugins.scala.lang.psi.types.ScType
import org.jetbrains.plugins.scala.lang.psi.types.api.FunctionType

import scala.annotation.tailrec

/**
  * Macro to materialize type level natural numbers from integer literals.
  */
object ShapelessMaterializeNat extends ScalaMacroTypeable with ShapelessUtils {
  override val boundMacro: Seq[MacroImpl] = MacroImpl("apply", "shapeless.Nat") :: Nil

  override def checkMacro(macros:  ScFunction, context: MacroContext): Option[ScType] = {
    context.place match {
      case lit @ ScIntLiteral(v) =>
        val resTpe = mkNatTpe(v, context.place)
        val anyTpe = lit.projectContext.stdTypes.Any
        resTpe.map(tpe => FunctionType((tpe, Seq(anyTpe)))(lit.elementScope))
      case _               => None
    }
  }

  private[this] val suc  = "_root_.shapeless.Succ"
  private[this] val zero = "_root_.shapeless._0"
  private[this] def mkNatTpe(i: Int, context: PsiElement): Option[ScType] = {
    @tailrec
    def loop(v: Int, acc: String): String =
      if (v == 0) acc
      else loop(v - 1, s"$suc[$acc]")

    val tpeText = loop(i, zero)
    ScalaPsiElementFactory.createTypeFromText(tpeText, context, null)
  }
}

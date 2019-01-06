package org.jetbrains.plugins.scala.lang.macros.evaluator.impl.records

import org.jetbrains.plugins.scala.lang.macros.evaluator.{MacroImpl, MacroInvocationContext, ScalaMacroExpandable}
import org.jetbrains.plugins.scala.lang.macros.evaluator.impl.ShapelessUtils
import org.jetbrains.plugins.scala.lang.psi.api.expr.ScExpression
import org.jetbrains.plugins.scala.lang.psi.api.statements.ScFunction
import org.jetbrains.plugins.scala.lang.psi.impl.ScalaPsiElementFactory

/**
  * Support for macro, which creates an empty record
  * {{{
  *   val e: HNil = Record()
  * }}}
  */
object ShapelessRecordApplyDynamic extends ScalaMacroExpandable with ShapelessUtils {
  override val boundMacro: Seq[MacroImpl] = MacroImpl("applyDynamic", "shapeless.record.Record") :: Nil

  override def expandMacro(macros:  ScFunction, context: MacroInvocationContext): Option[ScExpression] =
    if (context.call.argumentExpressions.nonEmpty) None
    else Option(ScalaPsiElementFactory.createExpressionFromText(fqHNil, context.call))
}

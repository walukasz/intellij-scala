package org.jetbrains.plugins.scala.lang.macros.evaluator.impl.records

import com.intellij.psi.PsiElement
import org.jetbrains.plugins.scala.lang.macros.evaluator.{MacroImpl, MacroInvocationContext, ScalaMacroExpandable}
import org.jetbrains.plugins.scala.lang.macros.evaluator.impl.ShapelessUtils
import org.jetbrains.plugins.scala.lang.macros.evaluator.impl.ShapelessUtils.SingletonSymbolTpe
import org.jetbrains.plugins.scala.lang.psi.api.expr.{ScAssignStmt, ScExpression, ScReferenceExpression}
import org.jetbrains.plugins.scala.lang.psi.api.statements.ScFunction
import org.jetbrains.plugins.scala.lang.psi.impl.ScalaPsiElementFactory

/**
  * Support for macro providing convinient record creation syntax a la
  * {{{
  * val rec = Record(a = 1, b = "foo")
  * }}} via `applyDynamicNamed`
  */
object ShapelessRecordApplyDynamicNamed extends ScalaMacroExpandable with ShapelessUtils {
  override val boundMacro: Seq[MacroImpl] = MacroImpl("applyDynamicNamed", "shapeless.record.Record") :: Nil

  override def expandMacro(macros: ScFunction, context: MacroInvocationContext): Option[ScExpression] = {
    val exprs = context.call.argumentExpressions
    mkRecord(exprs, context.call.getContext)
  }

  private[this] def mkRecord(args: Seq[ScExpression], context: PsiElement): Option[ScExpression] = {
    def mkElem(kTpeText: String, v: ScExpression): Option[String] = {
      val vTpe = v.`type`().map(_.widen)

      vTpe.map(tpe =>
        s"(${v.getText}).asInstanceOf[${FieldTpe(kTpeText, tpe.canonicalText)}]"
      ).toOption
    }

    def promoteElement(elem: ScExpression): Option[String] = elem match {
      case ScAssignStmt(ref: ScReferenceExpression, Some(v)) => mkElem(SingletonSymbolTpe(ref.refName), v)
      case _                                                 => None
    }

    val components = args.map(promoteElement)

    if (!components.forall(_.isDefined)) None
    else {
      val text = hListTextRaw(components.flatten, typePosition = false)
      Option(ScalaPsiElementFactory.createExpressionFromText(text, context))
    }
  }
}

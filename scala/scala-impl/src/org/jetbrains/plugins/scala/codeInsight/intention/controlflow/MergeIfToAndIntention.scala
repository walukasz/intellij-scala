package org.jetbrains.plugins.scala.codeInsight.intention.controlflow

import com.intellij.codeInsight.intention.PsiElementBaseIntentionAction
import com.intellij.openapi.editor.Editor
import com.intellij.openapi.project.Project
import com.intellij.psi.util.PsiTreeUtil
import com.intellij.psi.{PsiDocumentManager, PsiElement}
import org.jetbrains.plugins.scala.extensions._
import org.jetbrains.plugins.scala.lang.psi.api.expr._
import org.jetbrains.plugins.scala.lang.psi.impl.ScalaPsiElementFactory.createExpressionFromText

/**
 * @author Ksenia.Sautina
 * @since 6/6/12
 */

object MergeIfToAndIntention {
  def familyName = "Merge nested Ifs to ANDed condition"
}

class MergeIfToAndIntention extends PsiElementBaseIntentionAction {
  def getFamilyName: String = MergeIfToAndIntention.familyName

  override def getText: String = "Merge nested 'if's"

  def isAvailable(project: Project, editor: Editor, element: PsiElement): Boolean = {
    val ifStmt: ScIf = PsiTreeUtil.getParentOfType(element, classOf[ScIf], false)
    if (ifStmt == null) return false

    val offset = editor.getCaretModel.getOffset
    val thenBranch =  ifStmt.thenExpression.orNull
    val elseBranch =  ifStmt.elseExpression.orNull
    if (thenBranch == null || elseBranch != null) return false

    val condition = ifStmt.condition.orNull
    if (condition == null) return false

    if (!(ifStmt.getTextRange.getStartOffset <= offset &&
      offset <= condition.getTextRange.getStartOffset - 1)) return false

    thenBranch match {
      case branch: ScBlockExpr =>
        val exprs = branch.exprs
        if (exprs.size != 1 || !exprs(0).isInstanceOf[ScIf]) return false

        val innerIfStmt = exprs(0).asInstanceOf[ScIf]
        val innerElseBranch = innerIfStmt.elseExpression.orNull
        if (innerElseBranch != null) return false
        true

      case branch: ScIf =>
        val innerElseBranch = branch.elseExpression.orNull
        if (innerElseBranch != null) return false
        true

      case _ => false
    }
  }

  override def invoke(project: Project, editor: Editor, element: PsiElement) {
    val ifStmt : ScIf = PsiTreeUtil.getParentOfType(element, classOf[ScIf], false)
    if (ifStmt == null || !ifStmt.isValid) return

    val expr = new StringBuilder
    val outerCondition = ifStmt.condition.get.getText
    val innerIfStmt = ifStmt.thenExpression.get match {
      case c: ScBlockExpr => c.exprs(0).asInstanceOf[ScIf]
      case c: ScIf => c
    }
    val innerThenBranch = innerIfStmt.thenExpression.get
    val innerCondition = innerIfStmt.condition.get.getText

    expr.append("if (").append(outerCondition).append(" && ").append(innerCondition).append(") ").
      append(innerThenBranch.getText)

    inWriteAction {
      ifStmt.replaceExpression(createExpressionFromText(expr.toString())(element.getManager), true)
      PsiDocumentManager.getInstance(project).commitDocument(editor.getDocument)
    }
  }
}


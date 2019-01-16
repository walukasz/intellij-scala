package org.jetbrains.plugins.scala
package annotator

import com.intellij.lang.annotation.AnnotationHolder
import com.intellij.psi.{PsiClass, PsiField, PsiMethod}
import org.jetbrains.plugins.scala.annotator.AnnotatorUtils.registerTypeMismatchError
import org.jetbrains.plugins.scala.codeInspection.varCouldBeValInspection.ValToVarQuickFix
import org.jetbrains.plugins.scala.extensions._
import org.jetbrains.plugins.scala.lang.psi.ScalaPsiUtil
import org.jetbrains.plugins.scala.lang.psi.api.expr._
import org.jetbrains.plugins.scala.lang.psi.api.statements.params.ScClassParameter
import org.jetbrains.plugins.scala.lang.psi.api.statements.{ScFunction, ScValue, ScVariable}
import org.jetbrains.plugins.scala.lang.psi.types._
import org.jetbrains.plugins.scala.lang.resolve.processor.DynamicResolveProcessor
import org.jetbrains.plugins.scala.project.ProjectContext

/**
 * Pavel.Fatin, 31.05.2010
 */

trait AssignmentAnnotator {
  def annotateAssignment(assignment: ScAssignment, holder: AnnotationHolder, advancedHighlighting: Boolean) {
    implicit val ctx: ProjectContext = assignment

    val left = assignment.leftExpression
    val right = assignment.rightExpression

    assignment.leftExpression match {
      case _: ScMethodCall =>
      case ref: ScReferenceExpression =>
        ref.bind() match {
          case Some(r) if r.isDynamic && r.name == DynamicResolveProcessor.UPDATE_DYNAMIC => //ignore
          case Some(r) if !r.isNamedParameter =>
            def checkVariable() {
              left.`type`().foreach { lType =>
                right.foreach { expression =>
                  expression.getTypeAfterImplicitConversion().tr.foreach { rType =>
                    if(!ScalaPsiUtil.isUnderscoreEq(assignment, rType)) {
                      registerTypeMismatchError(rType, lType, holder, expression)
                    }
                  }
                }
              }
            }
            ScalaPsiUtil.nameContext(r.element) match {
              case _: ScVariable =>
                if (!advancedHighlighting) return
                checkVariable()
              case c: ScClassParameter if c.isVar =>
                if (!advancedHighlighting) return
                checkVariable()
              case f: PsiField if !f.hasModifierProperty("final") =>
                if (!advancedHighlighting) return
                checkVariable()
              case fun: ScFunction if ScalaPsiUtil.isViableForAssignmentFunction(fun) =>
                if (!advancedHighlighting) return
                assignment.resolveAssignment match {
                  case Some(ra) =>
                    ra.problems.foreach {
                      case TypeMismatch(expression, expectedType) =>
                        expression.`type`().foreach {
                          registerTypeMismatchError(_, expectedType, holder, expression)
                        }
                      case MissedValueParameter(_) => // simultaneously handled above
                      case UnresolvedParameter(_) => // don't show function inapplicability, unresolved
                      case WrongTypeParameterInferred => //todo: ?
                      case ExpectedTypeMismatch => // will be reported later
                      case _ => holder.createErrorAnnotation(assignment, "Wrong right assignment side")
                    }
                  case _ => holder.createErrorAnnotation(assignment, "Reassignment to val")
                }
              case _: ScFunction => holder.createErrorAnnotation(assignment, "Reassignment to val")
              case method: PsiMethod if method.getParameterList.getParametersCount == 0 =>
                method.containingClass match {
                  case c: PsiClass if c.isAnnotationType => //do nothing
                  case _ => holder.createErrorAnnotation(assignment, "Reassignment to val")
                }
              case _: ScValue =>
                val annotation = holder.createErrorAnnotation(assignment, "Reassignment to val")
                annotation.registerFix(new ValToVarQuickFix(ScalaPsiUtil.nameContext(r.element).asInstanceOf[ScValue]))
              case _ => holder.createErrorAnnotation(assignment, "Reassignment to val")
            }
          case _ =>
        }
      case _ =>
    }
  }
}

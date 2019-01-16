package org.jetbrains.plugins.scala.lang.psi.api.base

import com.intellij.psi.PsiAnnotation
import org.jetbrains.plugins.scala.lang.psi.api.ScalaPsiElement
import org.jetbrains.plugins.scala.lang.psi.api.base.types.ScTypeElement

/**
 * @author Alexander Podkhalyuzin
 * Date: 07.03.2008
 */

trait ScAnnotation extends ScalaPsiElement with PsiAnnotation {
  /**
   * Return full annotation only without @ token.
   * @return annotation expression
   */
  def annotationExpr: ScAnnotationExpr = findChildByClassScala(classOf[ScAnnotationExpr])

  /**
   * Return constructor element af annotation expression. For example
   * if annotation is <code>@Nullable</code> then method returns <code>
   * Nullable</code> psiElement.
   * @return constructor element
   */
  def constructor: ScConstructor = annotationExpr.constr

  def typeElement: ScTypeElement
}

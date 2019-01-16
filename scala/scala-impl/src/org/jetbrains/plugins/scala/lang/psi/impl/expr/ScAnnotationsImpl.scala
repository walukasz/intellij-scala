package org.jetbrains.plugins.scala
package lang
package psi
package impl
package expr

import com.intellij.lang.ASTNode
import org.jetbrains.plugins.scala.lang.parser.ScalaElementType
import org.jetbrains.plugins.scala.lang.psi.api.base.{ScAnnotation, ScAnnotations}
import org.jetbrains.plugins.scala.lang.psi.api.expr._
import org.jetbrains.plugins.scala.lang.psi.stubs.ScAnnotationsStub

/** 
* @author Alexander Podkhalyuzin
* Date: 07.03.2008
*/

class ScAnnotationsImpl private (stub: ScAnnotationsStub, node: ASTNode)
  extends ScalaStubBasedElementImpl(stub, ScalaElementType.ANNOTATIONS, node) with ScAnnotations {

  def this(node: ASTNode) = this(null, node)

  def this(stub: ScAnnotationsStub) = this(stub, null)

  override def toString: String = "AnnotationsList"

  def getAnnotations: Array[ScAnnotation] =
    getStubOrPsiChildren(ScalaElementType.ANNOTATION, JavaArrayFactoryUtil.ScAnnotationFactory)
}
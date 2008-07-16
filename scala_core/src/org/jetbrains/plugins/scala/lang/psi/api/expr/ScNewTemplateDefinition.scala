package org.jetbrains.plugins.scala.lang.psi.api.expr

import psi.ScalaPsiElement
import toplevel.templates.ScExtendsBlock

/** 
* @author Alexander Podkhalyuzin
* Date: 06.03.2008
*/

trait ScNewTemplateDefinition extends ScExpression {
  def extendsBlock = findChildByClass(classOf[ScExtendsBlock])
}
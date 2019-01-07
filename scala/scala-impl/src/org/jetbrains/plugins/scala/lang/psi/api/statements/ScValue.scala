package org.jetbrains.plugins.scala
package lang
package psi
package api
package statements

import com.intellij.psi.PsiElement
import javax.swing.Icon
import com.intellij.psi.tree.IElementType
import org.jetbrains.plugins.scala.icons.Icons
import org.jetbrains.plugins.scala.lang.lexer.ScalaTokenTypes.kVAL
import org.jetbrains.plugins.scala.lang.psi.api.expr.ScBlock
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.templates.ScExtendsBlock
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.typedef._

/**
 * @author Alexander Podkhalyuzin
 */
trait ScValue extends ScValueOrVariable with ScDecoratedIconOwner {
  override protected def keywordElementType: IElementType = kVAL

  override protected def isSimilarMemberForNavigation(member: ScMember, isStrict: Boolean): Boolean = member match {
    case other: ScValue => super.isSimilarMemberForNavigation(other, isStrict)
    case _ => false
  }

  // TODO unify with ScFunction and ScVariable
  override protected def getBaseIcon(flags: Int): Icon = {
    @scala.annotation.tailrec
    def basedOnParent(current: PsiElement): Icon = current match {
      case _: ScExtendsBlock         => if (isAbstract) Icons.ABSTRACT_FIELD_VAL else Icons.FIELD_VAL
      case _: ScBlock | _: ScalaFile => Icons.VAL
      case other if other != null    => basedOnParent(current.getParent)
      case _                         => Icons.VAL
    }
    basedOnParent(getParent)
  }

  def isAbstract: Boolean
}
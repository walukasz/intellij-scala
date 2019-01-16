package org.jetbrains.plugins.scala.lang.macros.expansion
import java.util.regex.Pattern

import com.intellij.openapi.project.Project
import com.intellij.psi.PsiElement
import org.jetbrains.plugin.scala.util.MacroExpansion
import org.jetbrains.plugins.scala.extensions
import org.jetbrains.plugins.scala.lang.psi.api.base.{ScAnnotation, ScAnnotationsHolder}
import org.jetbrains.plugins.scala.lang.psi.api.expr.{ScGenericCall, ScMethodCall}
import org.jetbrains.plugins.scala.lang.psi.impl.ScalaPsiElementFactory

import scala.annotation.tailrec

class ReflectExpansionLineMarkerProvider extends MacroExpansionLineMarkerProvider {

  override protected def getExpandMarker(element: PsiElement): Option[Marker] = {
    val collector = ReflectExpansionsCollector.getInstance(element.getProject)
     extensions.inReadAction {collector.getExpansion(element)} match {
      case Some(expansion) => Some(createExpandMarker(element)( _ => expandReflectMacro(element, expansion)))
      case None => None
    }
  }

  private def expandReflectMacro(element: PsiElement, expansion: MacroExpansion) = {
    def walkUp(elem: PsiElement, findAnnotee: Boolean = false): Option[PsiElement] = elem match {
      case null => None
      case a: ScAnnotation  => walkUp(a.getParent, findAnnotee = true)
      case ao: ScAnnotationsHolder if findAnnotee => Some(ao)
      case m: ScMethodCall  => Some(m)
      case m: ScGenericCall => Some(m)
      case e: PsiElement => walkUp(e.getParent, findAnnotee)
    }

    implicit val project: Project = element.getProject

    walkUp(element) match {
      case None    =>  //
      case Some(e) =>
        val fixed = ensugarExpansion(expansion.body)
        val psi = if (expansion.tree.startsWith("Block"))
          ScalaPsiElementFactory.createExpressionFromText(fixed)
        else
          ScalaPsiElementFactory.createBlockExpressionWithoutBracesFromText(fixed)
        reformatCode(psi)
        extensions.inWriteAction(e.replace(psi))
    }
  }

  override protected def getUndoMarker(element: PsiElement): Option[Marker] = None // TODO ?

  def ensugarExpansion(text: String): String = {
    @tailrec
    def applyRules(rules: Seq[(String, String)], input: String = text): String = {
      def pat(p: String) = Pattern.compile(p, Pattern.DOTALL | Pattern.MULTILINE)
      rules match {
        case (pattern, replacement) :: xs => applyRules(xs, pat(pattern).matcher(input).replaceAll(replacement))
        case Nil => input
      }
    }

    val rules = Seq(
      "\\<init\\>" -> "this", // replace constructor names
      " *\\<[a-z]+\\> *" -> "", // remove compiler attributes
      "super\\.this\\(\\);" -> "this();", // replace super constructor calls
      "def this\\(\\) = \\{\\s*this\\(\\);\\s*\\(\\)\\s*\\};" -> "", // remove invalid super constructor calls
      "_root_." -> "" // _root_ package is obsolete
    )

    applyRules(rules)
  }


}

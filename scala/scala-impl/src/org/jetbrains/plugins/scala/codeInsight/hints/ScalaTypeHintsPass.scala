package org.jetbrains.plugins.scala
package codeInsight
package hints

import java.lang.{Boolean => JBoolean}

import com.intellij.codeInsight.daemon.impl.HintRenderer
import com.intellij.codeInsight.hints.{ElementProcessingHintPass, ModificationStampHolder}
import com.intellij.openapi.editor.{Editor, EditorCustomElementRenderer, Inlay}
import com.intellij.openapi.util.Key
import com.intellij.openapi.vfs.VirtualFile
import com.intellij.psi.{PsiClass, PsiElement, PsiWhiteSpace}
import org.jetbrains.plugins.scala.extensions._
import org.jetbrains.plugins.scala.lang.psi.api.ScalaFile
import org.jetbrains.plugins.scala.lang.psi.api.expr._
import org.jetbrains.plugins.scala.lang.psi.api.statements.{ScFunction, ScValueOrVariable}
import org.jetbrains.plugins.scala.lang.psi.types.api.{JavaArrayType, ParameterizedType}
import org.jetbrains.plugins.scala.lang.psi.types.{ScCompoundType, ScType}
import org.jetbrains.plugins.scala.lang.refactoring.ScTypePresentationExt
import org.jetbrains.plugins.scala.settings.annotations.Definition.FunctionDefinition
import org.jetbrains.plugins.scala.settings.annotations._

import scala.annotation.tailrec

class ScalaTypeHintsPass(rootElement: ScalaFile,
                         editor: Editor,
                         stampHolder: ModificationStampHolder)
  extends ElementProcessingHintPass(rootElement, editor, stampHolder) {

  import ScalaTypeHintsPass._

  override def isAvailable(virtualFile: VirtualFile): Boolean = {
    val settings = ScalaCodeInsightSettings.getInstance
    settings.showFunctionReturnType || settings.showPropertyType || settings.showLocalVariableType || settings.showExpressionChainType
  }

  override def collectElementHints(element: PsiElement, collector: kotlin.jvm.functions.Function2[_ >: Integer, _ >: String, kotlin.Unit]): Unit = {
    implicit val settings: ScalaCodeInsightSettings = ScalaCodeInsightSettings.getInstance

    element match {
      case ReturnTypeAndBody(returnType, body, definition)
        if settings.showObviousType || !(definition.hasStableType || isObviousFor(body, returnType.extractClass)) =>

        for {
          anchor <- definition.parameterList
          offset = anchor.getTextRange.getEndOffset

          typeText <- typeToText(returnType)
          suffix = definition match {
            case FunctionDefinition(function) if !function.hasAssign && function.hasUnitResultType => " ="
            case _ => ""
          }
          typeInfo = s": " + typeText + suffix
        } collector.invoke(offset, typeInfo)

      case ExprChain(chain) if chain.size > 2 =>
        val perLine =
          chain.filter(isFollowedByLineBreak)

        if (perLine.size < 3) {
          return None
        }

        val types =
          perLine
          .map(e => e.`type`())
          .takeWhile { _.isRight }
          .map(_.right.get)
          .toList

        // don't show chains that are too simple
        if (types.toSet.size < 2) {
          return None
        }

        var last: String = null
        for {
          (expr, ty) <- perLine zip types
          offset = expr.getTextRange.getEndOffset
          typeInfo <- typeToText(ty)
          if typeInfo != last
        } {
          collector.invoke(offset, ": " + typeInfo)
          last = typeInfo
        }

      case _ => None
    }
  }

  override def getHintKey: Key[JBoolean] = ScalaTypeInlayKey

  override def createRenderer(text: String): HintRenderer = new HintRenderer(text) {
    override def getContextMenuGroupId(inlay: Inlay[_ <: EditorCustomElementRenderer]): String = "TypeHintsMenu"
  }
}

object ScalaTypeHintsPass {

  import Definition._

  private val ScalaTypeInlayKey = Key.create[JBoolean]("SCALA_TYPE_INLAY_KEY")

  private object ExprChain {
    def unapply(element: PsiElement)(implicit settings: ScalaCodeInsightSettings): Option[Seq[ScExpression]] = {
      element match {
        case expr: ScExpression if settings.showExpressionChainType && isMostOuterExpression(expr)=>
          Some(collectChain(expr))
        case _ => None
      }
    }

    private def isMostOuterExpression(expr: ScExpression): Boolean = {
      expr.parent.exists {
        case _: ScReferenceExpression => false
        case _: MethodInvocation => false
        case _ => true
      }
    }

    private def collectChain(expr: ScExpression): List[ScExpression] = {
      @tailrec
      def collectChainAcc(expr: ScExpression, acc: List[ScExpression]): List[ScExpression] = {
        val newAcc = expr :: acc
        expr match {
          case ChainCall(inner) => collectChainAcc(inner, newAcc)
          case _ => newAcc
        }
      }
      collectChainAcc(expr, Nil)
    }

    object ChainCall {
      def unapply(element: PsiElement): Option[ScExpression] = element match {
        case ScReferenceExpression.withQualifier(inner) => Some(inner)
        case MethodInvocation(inner, _) => Some(inner)
        case ScInfixExpr(inner, _, _) => Some(inner)
        case ScPrefixExpr(_, inner) => Some(inner)
        case ScPostfixExpr(inner, _) => Some(inner)
        case _ => None
      }
    }
  }

  private object ReturnTypeAndBody {

    def unapply(element: PsiElement)
               (implicit settings: ScalaCodeInsightSettings): Option[(ScType, ScExpression, Definition)] = {
      val definition = Definition(element)
      for {
        body <- definition.bodyCandidate
        returnType <- definition match {
          case ValueDefinition(value) => unapply(value)
          case VariableDefinition(variable) => unapply(variable)
          case FunctionDefinition(function) => unapply(function)
          case _ => None
        }
      } yield (returnType, body, definition)
    }

    private def unapply(member: ScValueOrVariable)
                       (implicit settings: ScalaCodeInsightSettings) = {
      val flag = if (member.isLocal) settings.showLocalVariableType
      else settings.showPropertyType

      if (flag) member.`type`().toOption else None
    }

    private def unapply(member: ScFunction)
                       (implicit settings: ScalaCodeInsightSettings) =
      if (settings.showFunctionReturnType) member.returnType.toOption
      else None
  }

  private def isObviousFor(body: ScExpression, maybeClass: Option[PsiClass]) =
    maybeClass.map {
      _.name -> body
    }.exists {
      case (className, ReferenceName(name, _)) =>
        !name.mismatchesCamelCase(className)
      case _ => false
    }

  private def typeToText(`type`: ScType)(implicit settings: ScalaCodeInsightSettings): Option[String] = {
    `type` match {
      case CodeTextOfType(text) if limited(text) => Some(text)
      case FoldedCodeText(text) if limited(text) => Some(text)
      case _ => None
    }
  }

  private def limited(text: String)
                     (implicit settings: ScalaCodeInsightSettings): Boolean =
    text.length <= settings.presentationLength

  private object CodeTextOfType {

    def unapply(`type`: ScType) = Some(`type`.codeText)
  }

  private object FoldedCodeText {

    private[this] val Ellipsis = "..."

    def unapply(`type`: ScType): Option[String] = `type` match {
      case ScCompoundType(components, signatures, types) =>
        val suffix = if (signatures.nonEmpty || types.nonEmpty) s" {$Ellipsis}" else ""
        val text = components match {
          case Seq(CodeTextOfType(head), _, _*) => s"$head with $Ellipsis"
          case Seq(CodeTextOfType(head)) => head + suffix
          case Seq() => "AnyRef" + suffix
        }
        Some(text)
      case ParameterizedType(CodeTextOfType(text), typeArguments) =>
        val suffix = Seq.fill(typeArguments.size)(Ellipsis)
          .commaSeparated(model = Model.SquareBrackets)
        Some(text + suffix)
      case JavaArrayType(_) => Some(s"Array[$Ellipsis]")
      case _ => None
    }
  }

  def isFollowedByLineBreak(elem: PsiElement): Boolean =
    elem.nextSibling.exists {
      case ws: PsiWhiteSpace => ws.textContains('\n')
      case _ => false
    }
}

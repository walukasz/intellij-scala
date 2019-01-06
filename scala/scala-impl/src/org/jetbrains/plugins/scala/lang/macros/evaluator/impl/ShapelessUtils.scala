package org.jetbrains.plugins.scala.lang.macros.evaluator.impl

import com.intellij.psi.PsiElement
import org.jetbrains.plugins.scala.extensions.PsiElementExt
import org.jetbrains.plugins.scala.lang.macros.evaluator.MacroContext
import org.jetbrains.plugins.scala.lang.macros.evaluator.impl.ShapelessUtils.SingletonSymbolTpe
import org.jetbrains.plugins.scala.lang.psi.api.base.ScStableCodeReferenceElement
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.typedef.ScClass
import org.jetbrains.plugins.scala.lang.psi.impl.ScalaPsiElementFactory
import org.jetbrains.plugins.scala.lang.psi.types.api.designator.{ScDesignatorType, ScProjectionType}
import org.jetbrains.plugins.scala.lang.psi.types.api.{ParameterizedType, StdTypes}
import org.jetbrains.plugins.scala.lang.psi.types.{ScCompoundType, ScLiteralType, ScParameterizedType, ScType}
import org.jetbrains.plugins.scala.lang.refactoring.util.ScalaNamesUtil

trait ShapelessUtils {

  private[macros] val fqDefSymLab  = "_root_.shapeless.DefaultSymbolicLabelling"
  private[macros] val fqFieldType  = "_root_.shapeless.labelled.FieldType"
  private[macros] val fqKeyTag     = "_root_.shapeless.labelled.KeyTag"
  private[macros] val fqTagged     = "_root_.shapeless.tag.Tagged"
  private[macros] val fqGeneric    = "_root_.shapeless.Generic"
  private[macros] val fqColonColon = "_root_.shapeless.::"
  private[macros] val fqHNil       = "_root_.shapeless.HNil"

  private val tupleN = "Tuple"

  /**
    *  Target classed of <Labelled>Generic.Aux[L, K, ...] are encoded as the first argument
    */
  protected def extractTargetType(context: MacroContext): ScType = context.expectedType.get match {
    case t: ScParameterizedType => t.typeArguments.head
    case _ => StdTypes.instance(context.place.projectContext).Any
  }

  private def extractFiledsFromClass(c: ScClass):Seq[(String, ScType)] = {
    c.constructor.map(_.parameters.map(p=> (p.name, p.`type`().getOrAny))).getOrElse(Seq.empty)
  }

  /**
    * Extracts case class field names and types or element types from tuples to be used by HList generator
    */
  protected def extractFields(tp: ScType): Seq[(String, ScType)] = tp match {
    case ParameterizedType(ScDesignatorType(c: ScClass), args) if c.name.startsWith(tupleN) =>
      args.zipWithIndex.map{e => (s"_${e._2}", e._1)}
    case ScDesignatorType(c: ScClass) if c.isCase =>
      extractFiledsFromClass(c)
    case ScProjectionType(_, elem: ScClass) if elem.isCase =>
      extractFiledsFromClass(elem)
    case _ => Seq.empty
  }

  protected def hlistText(componentTypes: Seq[ScType]): String =
    hListTextRaw(componentTypes.map(_.canonicalText))

  protected def hListTextRaw(componentTexts: Seq[String], typePosition: Boolean = true): String = {
    val wrap: (String, String) => String =
      (cur, acc) => if (typePosition) s"[$cur, $acc]" else s"($cur, $acc)"
    componentTexts.foldRight(fqHNil)((p, suffix) => s"($fqColonColon${wrap(p, suffix)}")
  }

  protected def backtickedLiteralIn(e: PsiElement): Option[String] = {
    val ref = e match {
      case r: ScStableCodeReferenceElement => r
      case _ => return None
    }
    ref.refName match {
      case ScalaNamesUtil.isBacktickedName(literalText) =>
        parseLiteralType(literalText, ref.getContext, ref).map {
          case lit: ScLiteralType => ScLiteralType.printValue(lit)
          case other              => other.canonicalText // singleton symbol type
        }
      case _ => None
    }
  }

  protected def parseLiteralType(text: String, context: PsiElement, child: PsiElement): Option[ScType] =
    ScalaPsiElementFactory.createExpressionWithContextFromText(text, context, child).getNonValueType() match {
      case Right(ScLiteralType(sym: Symbol)) => SingletonSymbolTpe(sym.name, context)
      case Right(lit: ScLiteralType)         => Option(lit)
      case _                                 => None
    }
}

object ShapelessUtils extends ShapelessUtils {
  private[macros] final case class TargetOrAux[A](targetExtractor: PartialFunction[ScParameterizedType, A]) {
    def unapply(tp: ScType): Option[A] = tp.removeAliasDefinitions() match {
      case ScCompoundType(Seq(tpe: ScParameterizedType), _, _) => targetExtractor.lift(tpe)
      case tpe: ScParameterizedType                            => targetExtractor.lift(tpe)
      case _                                                   => None
    }
  }

  private[macros] object SingletonSymbolTpe {
    def apply(name: String, context: PsiElement): Option[ScType] =
      ScalaPsiElementFactory.createTypeFromText(apply(name), context, null)

    def apply(name: String): String = s"""Symbol with $fqTagged["$name"]"""

    def unapply(tpe: ScCompoundType): Option[ScLiteralType] = tpe match {
      case ScCompoundType(Seq(SymTpe(), TaggedTpe(arg)), _, _) => Option(arg)
      case _                                                   => None
    }
  }

  private[macros] object SymTpe {
    def unapply(tpe: ScDesignatorType): Boolean = tpe match {
      case ScDesignatorType(cls: ScClass) => cls.qualifiedName == "scala.Symbol"
      case _                              => false
    }
  }

  private[macros] object TaggedTpe {
    def unapply(tpe: ScParameterizedType): Option[ScLiteralType] = tpe match {
      case ParameterizedType(des, Seq(litTpe: ScLiteralType))
        if des.canonicalText == fqTagged => Option(litTpe)
      case _ => None
    }
  }
}

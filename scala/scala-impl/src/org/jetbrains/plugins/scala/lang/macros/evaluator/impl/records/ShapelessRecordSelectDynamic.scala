package org.jetbrains.plugins.scala.lang.macros.evaluator.impl.records

import com.intellij.psi.PsiElement
import org.jetbrains.plugins.scala.lang.macros.evaluator.impl.ShapelessUtils
import org.jetbrains.plugins.scala.lang.macros.evaluator.{MacroContext, MacroImpl, ScalaMacroTypeable}
import org.jetbrains.plugins.scala.lang.psi.api.base.ScStableCodeReferenceElement
import org.jetbrains.plugins.scala.lang.psi.api.statements.ScFunction
import org.jetbrains.plugins.scala.lang.psi.impl.ScalaPsiElementFactory.createTypeFromText
import org.jetbrains.plugins.scala.lang.psi.types.ScType
import org.jetbrains.plugins.scala.lang.refactoring.util.ScalaNamesUtil


/**
  * A concise syntax to represent record types e.g.
  * {{{
  * type Xyz = Record.`'x -> Int, 'y -> String, 'z -> Boolean`.T
  * }}}
  */
object ShapelessRecordSelectDynamic extends ScalaMacroTypeable with ShapelessUtils {
  override val boundMacro: Seq[MacroImpl] = MacroImpl("selectDynamic", "shapeless.record.Record") :: Nil

  override def checkMacro(macros: ScFunction, context: MacroContext): Option[ScType] = {
    val refText = context.place match {
      case ref: ScStableCodeReferenceElement =>
        ref.refName match {
          case ScalaNamesUtil.isBacktickedName(text) => text
          case _                                     => ""
        }
      case _ => ""
    }

    val fields = refText.split(",").map(_.trim).map(_.split("->").map(_.trim)).map {
      case Array(key, value) =>
        val keyTpe   = parseLiteralType(key, context.place, null).getOrElse(return None)
        val valueTpe = createTypeFromText(value, context.place, null).getOrElse(return None)
        FieldTpe(keyTpe.canonicalText, valueTpe.canonicalText)
      case _ => return None
    }

    val tpeText = hListTextRaw(fields)
    mkTypeCarrier(tpeText, macros)
  }

  private[this] def mkTypeCarrier(tpeText: String, context: PsiElement): Option[ScType] =
    createTypeFromText(s"{ type T = $tpeText }", context, null)
}

package org.jetbrains.plugins.scala.lang.macros.evaluator.impl.records

import org.jetbrains.plugins.scala.lang.macros.evaluator.impl.{ShapelessUtils, TargetTypeHasAux}
import org.jetbrains.plugins.scala.lang.macros.evaluator.{MacroContext, MacroImpl, ScalaMacroTypeable}
import org.jetbrains.plugins.scala.lang.psi.api.statements.ScFunction
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.typedef.ScTrait
import org.jetbrains.plugins.scala.lang.psi.impl.ScalaPsiElementFactory
import org.jetbrains.plugins.scala.lang.psi.types.api.ParameterizedType
import org.jetbrains.plugins.scala.lang.psi.types.api.designator.ScDesignatorType
import org.jetbrains.plugins.scala.lang.psi.types.{ScParameterizedType, ScType}

/**
  * Base class for a family of common macro materializers related to
  * extensible records from shapeless: mk(Selector/Remover/Updater/Modifier).
  */
trait ShapelessRecordMacro[A] extends ScalaMacroTypeable with ShapelessUtils with TargetTypeHasAux[A] {
  protected val pkg = "shapeless.ops.record"

  protected def classSimpleName: String
  protected def processType(extracted: A): Option[String]

  protected def macroName: String = s"mk$classSimpleName"
  protected def classFqn: String  = s"_root_.$pkg.$classSimpleName"

  override val boundMacro: Seq[MacroImpl] = MacroImpl(macroName, s"$pkg.$classSimpleName") :: Nil

 /**
   * Extracts type arguments from [[ParameterizedType]] dented by [[classSimpleName]]
   */
  protected object TargetTpeExtractor {
    def unapplySeq(tpe: ScParameterizedType): Option[Seq[ScType]] = tpe match {
      case ParameterizedType(ScDesignatorType(cls: ScTrait), tparams)
        if cls.qualifiedName == s"$pkg.$classSimpleName" => Option(tparams)
      case _ => None
    }
  }

  override def checkMacro(macros: ScFunction, context: MacroContext): Option[ScType] =
    context.expectedType
      .map(_.removeAliasDefinitions())
      .collect { case targetOrAux(tpe) => tpe }
      .flatMap(processType)
      .flatMap(ScalaPsiElementFactory.createTypeFromText(_, context.place, null))
}



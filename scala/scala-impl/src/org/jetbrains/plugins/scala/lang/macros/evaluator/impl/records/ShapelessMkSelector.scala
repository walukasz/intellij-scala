package org.jetbrains.plugins.scala.lang.macros.evaluator.impl.records

import org.jetbrains.plugins.scala.lang.psi.types.ScType

/**
  * Macro materializer of `Selector` typeclass instances, supporting
  * record field selection.
  */
object ShapelessMkSelector extends ShapelessRecordMacro[(ScType, ScType)] {
  override protected def classSimpleName: String = "Selector"

  override protected def targetExtractor: PartialFunction[ScType, (ScType, ScType)] = {
    case TargetTpeExtractor(lTpe, kTpe) => (lTpe, kTpe)
  }

  override protected def processType(extracted: (ScType, ScType)): Option[String] = {
    val (lTpe, kTpe) = extracted

    findField(lTpe, kTpe)
      .map { case (fieldTpe, _) =>
        s"$classFqn.Aux[${lTpe.canonicalText}, ${kTpe.canonicalText}, $fieldTpe]"
      }
  }
}

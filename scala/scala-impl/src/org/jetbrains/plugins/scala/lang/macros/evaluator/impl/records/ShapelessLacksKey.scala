package org.jetbrains.plugins.scala.lang.macros.evaluator.impl.records

import org.jetbrains.plugins.scala.lang.psi.types.ScType


/**
  * Macro materializer of `LacksKey` typeclass instances, providing evidence
  * that given record *DOES NOT* contain given field.
  */
object ShapelessLacksKey extends ShapelessRecordMacro[(ScType, ScType)] {
  override protected def classSimpleName: String = "LacksKey"
  override protected def macroName: String       = "apply"

  override protected def targetExtractor: PartialFunction[ScType, (ScType, ScType)] = {
    case TargetTpeExtractor(lTpe, kTpe) => (lTpe, kTpe)
  }

  override protected def processType(extracted: (ScType, ScType)): Option[String] = {
    val (lTpe, kTpe) = extracted

    findField(lTpe, kTpe) match {
      case None => Option(s"$classFqn[${lTpe.canonicalText}, ${kTpe.canonicalText}]")
      case _    => None
    }
  }
}

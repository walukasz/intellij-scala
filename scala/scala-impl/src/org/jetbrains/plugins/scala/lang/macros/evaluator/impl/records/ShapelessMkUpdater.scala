package org.jetbrains.plugins.scala.lang.macros.evaluator.impl.records

import org.jetbrains.plugins.scala.lang.psi.types.ScType

/**
  * Macro materializer of `Updater` typeclass instances, supporting
  * record update and extension.
  */
object ShapelessMkUpdater extends ShapelessRecordMacro[(ScType, ScType)] {
  override protected def classSimpleName: String = "Updater"

  override protected def targetExtractor: PartialFunction[ScType, (ScType, ScType)] = {
    case TargetTpeExtractor(lTpe, kTpe) => (lTpe, kTpe)
  }

  override protected def processType(extracted: (ScType, ScType)): Option[String] = {
    val (lTpe, kTpe) = extracted
    val components   = unpackHListType(lTpe)

    val updatedComponentsText = {
      val i = components.indexWhere(_ equiv kTpe)

      val upd =
        if (i < 0) components :+ kTpe
        else       components.updated(i, kTpe)

      hlistText(upd)
    }

    Option(s"$classFqn.Aux[${lTpe.canonicalText}, ${kTpe.canonicalText}, $updatedComponentsText]")
  }
}

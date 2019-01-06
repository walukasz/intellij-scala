package org.jetbrains.plugins.scala.lang.macros.evaluator.impl.records

import org.jetbrains.plugins.scala.lang.psi.types.ScType

/**
  * Macro materializer of `Remover` typeclass instances, supporting
  * record field removal.
  */
object ShapelessMkRemover extends ShapelessRecordMacro[(ScType, ScType)] {
  override protected def classSimpleName: String = "Remover"

  override protected def targetExtractor: PartialFunction[ScType, (ScType, ScType)] = {
    case TargetTpeExtractor(lTpe, kTpe) => (lTpe, kTpe)
  }

  override protected def processType(extracted: (ScType, ScType)): Option[String] = {
    val (lTpe, kTpe) = extracted

    findField(lTpe, kTpe).map {
      case (field, idx) =>
        val (prefix, List(_, suffix @ _*)) = unpackHListType(lTpe).splitAt(idx)
        val out                            = hlistText(prefix ++ suffix)
        s"$classFqn.Aux[${lTpe.canonicalText}, ${kTpe.canonicalText}, (${field.canonicalText}, $out)]"
    }
  }
}

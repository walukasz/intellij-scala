package org.jetbrains.plugins.scala.lang.macros.evaluator.impl.records

import org.jetbrains.plugins.scala.lang.psi.types.ScType

/**
  * Macro materializer of `Mofidier` typeclass instances, supporting
  * modification of a record field by given function.
  */
object ShapelessMkModifier extends ShapelessRecordMacro[(ScType, ScType, ScType, ScType)] {
  type Extracted = (ScType, ScType, ScType, ScType)

  override protected def classSimpleName: String = "Modifier"

  override protected def targetExtractor: PartialFunction[ScType, Extracted] = {
    case TargetTpeExtractor(lTpe, kTpe, domain, codomain) => (lTpe, kTpe, domain, codomain)
  }

  override protected def processType(arg: Extracted): Option[String] = {
    val (lTpe, kTpe, fDomain, fCodomain) = arg

    findField(lTpe, kTpe).collect {
      case (field, idx) if field.conforms(fDomain) =>
        val (prefix, List(_, suffix @ _*)) = unpackHListType(lTpe).splitAt(idx)

        val out = hListTextRaw(
          prefix.map(_.canonicalText) ++
            (FieldTpe(field.canonicalText, fCodomain.canonicalText) +: suffix.map(_.canonicalText))
        )

        s"""$classFqn.Aux[
           |  ${lTpe.canonicalText}, ${kTpe.canonicalText},
           |  ${fDomain.canonicalText}, ${fCodomain.canonicalText}, $out
           ]""".stripMargin
    }
  }
}

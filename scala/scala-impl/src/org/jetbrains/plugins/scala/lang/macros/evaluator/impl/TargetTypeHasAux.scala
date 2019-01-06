package org.jetbrains.plugins.scala.lang.macros.evaluator.impl

import org.jetbrains.plugins.scala.lang.macros.evaluator.impl.ShapelessUtils.TargetOrAux
import org.jetbrains.plugins.scala.lang.psi.types.ScType

trait TargetTypeHasAux[A] {
  protected def targetExtractor: PartialFunction[ScType, A]

  protected val targetOrAux: TargetOrAux[A] = TargetOrAux(targetExtractor)
}

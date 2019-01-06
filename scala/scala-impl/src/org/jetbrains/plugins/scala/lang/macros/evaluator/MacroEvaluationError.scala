package org.jetbrains.plugins.scala.lang.macros.evaluator

sealed trait MacroEvaluationError

object MacroEvaluationError {
  case object NoRuleDefined    extends MacroEvaluationError
  case object MacroCheckFailed extends MacroEvaluationError
}

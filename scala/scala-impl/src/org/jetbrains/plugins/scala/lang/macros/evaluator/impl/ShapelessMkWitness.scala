package org.jetbrains.plugins.scala.lang.macros.evaluator.impl

import com.intellij.psi.PsiElement
import org.jetbrains.plugins.scala.extensions._
import org.jetbrains.plugins.scala.lang.macros.evaluator.impl.ShapelessUtils.SingletonSymbolTpe
import org.jetbrains.plugins.scala.lang.macros.evaluator.{MacroContext, MacroImpl, ScalaMacroTypeable}
import org.jetbrains.plugins.scala.lang.psi.api.statements.ScFunction
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.typedef.ScTrait
import org.jetbrains.plugins.scala.lang.psi.impl.ScalaPsiElementFactory
import org.jetbrains.plugins.scala.lang.psi.types.api.FunctionType
import org.jetbrains.plugins.scala.lang.psi.types.api.designator.{DesignatorOwner, ScDesignatorType, ScThisType}
import org.jetbrains.plugins.scala.lang.psi.types.result.Typeable
import org.jetbrains.plugins.scala.lang.psi.types.{ScCompoundType, ScLiteralType, ScType}

object ShapelessMkWitness extends ScalaMacroTypeable with ShapelessUtils {
  private[this] val witnessFqn = "shapeless.Witness"

  override val boundMacro: Seq[MacroImpl] = MacroImpl("apply", witnessFqn) :: Nil

  override def checkMacro(macros: ScFunction, context: MacroContext): Option[ScType] = {
    val expectedType = context.expectedType.map(_.removeAliasDefinitions()).flatMap {
      case WitnessTpe(expected) => Option(expected)
      case _ =>
        context.place match {
          case Typeable(tpe) => Option(tpe)
          case _             => None
        }
    }

    val witnessTpe = for {
      expected     <- expectedType
      singletonTpe <- extractSingletonType(expected, context.place)
      witness      <- generateWitnessAux(singletonTpe, context.place)
    } yield witness

    if (macros.parameters.isEmpty) witnessTpe
    else {
      val anyTpe = context.place.projectContext.stdTypes.Any
      witnessTpe.map(tpe => FunctionType((tpe, Seq(anyTpe)))(context.place.elementScope))
    }
  }

  private[this] def extractSingletonType(tpe: ScType, context: PsiElement): Option[ScType] = tpe match {
    case ScLiteralType(sym: Symbol)                 => SingletonSymbolTpe(sym.name, context)
    case lit: ScLiteralType                         => Option(lit)
    case downer: DesignatorOwner if downer.isStable => Option(downer)
    case thisTpe: ScThisType                        => Option(thisTpe)
    case tagged @ SingletonSymbolTpe(_)             => Option(tagged)
    case _                                          => None
  }

  private object WitnessTpe {
    def unapply(tpe: ScCompoundType): Option[ScType] = tpe match {
      case ScCompoundType(Seq(ScDesignatorType(trt: ScTrait)), _, types)
        if trt.qualifiedName == "shapeless.Witness" && types.size == 1 =>
        val aliasSig = types.head._2
        if (aliasSig.isDefinition) Option(aliasSig.lowerBound)
        else                       None
      case _ => None
    }
  }

  private[this] def generateWitnessAux(singletonType: ScType, context: PsiElement): Option[ScType] = {
    val tpeText = singletonType.canonicalText
    ScalaPsiElementFactory.createTypeFromText(s"$witnessFqn.Aux[$tpeText]", context, null)
  }
}

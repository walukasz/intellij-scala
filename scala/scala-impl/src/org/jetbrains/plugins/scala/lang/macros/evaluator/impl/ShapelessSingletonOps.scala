package org.jetbrains.plugins.scala.lang.macros.evaluator.impl

import com.intellij.psi.PsiElement
import org.jetbrains.plugins.scala.lang.macros.evaluator.{MacroContext, MacroImpl, ScalaMacroTypeable}
import org.jetbrains.plugins.scala.lang.psi.api.base.ScLiteral
import org.jetbrains.plugins.scala.lang.psi.api.statements.ScFunction
import org.jetbrains.plugins.scala.lang.psi.impl.ScalaPsiElementFactory
import org.jetbrains.plugins.scala.lang.psi.types.{ScLiteralType, ScType, ScalaType}
import org.jetbrains.plugins.scala.extensions._
import org.jetbrains.plugins.scala.lang.macros.evaluator.impl.ShapelessUtils.SingletonSymbolTpe
import org.jetbrains.plugins.scala.lang.psi.types.api.FunctionType
import org.jetbrains.plugins.scala.project.ProjectContext

/**
  * Macro for convinient singleton generation from terms, e.g. in
  * {{{
  * val book =
  *   ("author" ->> "Benjamin Pierce") ::
  *     ("title" ->> "Types and Programming Languages") ::
  *     ("id" ->> 262162091) ::
  *     ("price" ->> 44.11) ::
  *     HNil
  * }}}
  * "author", "title", "id" and "price" tags are implicitly converted to corresponding `SingletonOps`
  * with correct literal types.
  */
object ShapelessSingletonOps extends ScalaMacroTypeable with ShapelessUtils {
  override val boundMacro: Seq[MacroImpl] = MacroImpl("mkSingletonOps", "shapeless.syntax.singleton") :: Nil

  override def checkMacro(macros: ScFunction, context: MacroContext): Option[ScType] = {
    val place = context.place

    place match {
      case ScLiteral(sym: Symbol) =>
        SingletonSymbolTpe(sym.name, place).flatMap(typeCarrierType(_, place))
      case ScLiteral(value) =>
        ScLiteralType.fromValue(value)(place.projectContext).flatMap(typeCarrierType(_, place))
      case _ => None
    }
  }

  private def typeCarrierType(keyTpe: ScType, insertionPlace: PsiElement): Option[ScType] = {
    val witnessText = keyTpe match {
      case lit: ScLiteralType => ScLiteralType.printValue(lit)
      case other              => other.canonicalText
    }

    val text =
      s"""
         |final class `${witnessText}SingletonOps` extends _root_.shapeless.syntax.SingletonOps {
         |  type T = $witnessText
         |  val witness: Witness.Aux[$witnessText] = $witnessText
         |}""".stripMargin

    val typeCarrier = ScalaPsiElementFactory.createTypeDefinitionWithContext(text, insertionPlace.getContext, insertionPlace)
    val retTpe = ScalaType.designator(typeCarrier)

    val project: ProjectContext = insertionPlace.getProject
    import project.stdTypes

    FunctionType((retTpe, Seq(stdTypes.Any)))(insertionPlace.elementScope).toOption
  }
}

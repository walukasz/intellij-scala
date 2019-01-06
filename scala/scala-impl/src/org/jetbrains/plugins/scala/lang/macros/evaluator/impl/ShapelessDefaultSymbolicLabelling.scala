package org.jetbrains.plugins.scala.lang.macros.evaluator.impl
import org.jetbrains.plugins.scala.lang.macros.evaluator.impl.ShapelessUtils.SingletonSymbolTpe
import org.jetbrains.plugins.scala.lang.macros.evaluator.{MacroContext, MacroImpl, ScalaMacroTypeable}
import org.jetbrains.plugins.scala.lang.psi.api.statements.ScFunction
import org.jetbrains.plugins.scala.lang.psi.impl.ScalaPsiElementFactory
import org.jetbrains.plugins.scala.lang.psi.types.ScType

/**
  * Generates type signatures that serialize _named_ case class fields
  *
  * example:
  * case class Foo(age: Int, name: String) ->
  * -> DefaultSymbolicLabelling.Aux[Main.this.Foo, (Symbol @@ String("age")) :: (Symbol @@ String("name")) :: shapeless.HNil])
  */
object ShapelessDefaultSymbolicLabelling extends ScalaMacroTypeable with ShapelessUtils {

  override val boundMacro: Seq[MacroImpl] = MacroImpl("mkDefaultSymbolicLabelling", "shapeless.DefaultSymbolicLabelling") :: Nil

  override def checkMacro(macros: ScFunction, context: MacroContext): Option[ScType] = {
    if (context.expectedType.isEmpty) return None
    val targetClass = extractTargetType(context)
    val fields      = extractFields(targetClass)
    val reprTpStr   = fields.foldRight(fqHNil) { case ((pName, _), suffix) =>
      s"$fqColonColon[${SingletonSymbolTpe(pName)}, $suffix]"
    }
    val genericStr  = s"$fqDefSymLab.Aux[${targetClass.canonicalText}, $reprTpStr]"
    ScalaPsiElementFactory.createTypeFromText(genericStr, context.place, null)
  }
}

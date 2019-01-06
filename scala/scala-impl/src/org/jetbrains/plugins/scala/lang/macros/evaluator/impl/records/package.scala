package org.jetbrains.plugins.scala.lang.macros.evaluator.impl

import org.jetbrains.plugins.scala.lang.macros.evaluator.impl.ShapelessUtils._
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.typedef.ScTemplateDefinition
import org.jetbrains.plugins.scala.lang.psi.types.api.ParameterizedType
import org.jetbrains.plugins.scala.lang.psi.types.api.designator.ScDesignatorType
import org.jetbrains.plugins.scala.lang.psi.types.{ScCompoundType, ScType}

import scala.annotation.tailrec

package object records {
  object FieldTpe {
    def apply(kTpeText: String, vTpeText: String): String =
      s"$fqFieldType[$kTpeText, $vTpeText]"

    def unapply(tpe: ScCompoundType): Option[(ScType, ScType)] = tpe match {
      case ScCompoundType(Seq(fieldTpe, ParameterizedType(kTagDes, Seq(tp, _))), _, _)
          if kTagDes.canonicalText == ShapelessUtils.fqKeyTag =>
        Option((tp, fieldTpe))
      case _ => None
    }
  }

  object HNil {
    def unapply(tpe: ScDesignatorType): Boolean = tpe match {
      case ScDesignatorType(hnil: ScTemplateDefinition) => hnil.qualifiedName == "shapeless.HNil"
      case _                                            => false
    }
  }

  def findField(recordType: ScType, keyTpe: ScType): Option[(ScType, Int)] =
    unpackHListType(recordType).zipWithIndex.collectFirst {
      case (FieldTpe(kTpe, fTpe), idx) if kTpe equiv keyTpe => (fTpe, idx)
    }

  def unpackHListType(lTpe: ScType): List[ScType] = {
    @tailrec
    def unfold(tpe: ScType, acc: List[ScType]): List[ScType] = tpe match {
      case ParameterizedType(des, Seq(hd, tl)) if des.canonicalText == fqColonColon => unfold(tl, hd :: acc)
      case HNil()                                                                   => acc
      case _                                                                        => acc
    }

    unfold(lTpe, Nil).reverse
  }
}

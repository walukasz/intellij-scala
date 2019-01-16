package scala.meta

import com.intellij.openapi.project.Project
import com.intellij.openapi.vfs.VirtualFileManager
import com.intellij.psi.{PsiFile, PsiManager}
import org.jetbrains.plugins.scala.lang.psi.api.base.ScAnnotation

trait EnvironmentProvider {
  def findFileByPath(path: String): PsiFile = {
    val virtualFile = VirtualFileManager.getInstance().findFileByUrl(path)
    PsiManager.getInstance(getCurrentProject).findFile(virtualFile)
  }
  def getCurrentProject: Project

  def dumbMode: Boolean = false

  // at the time this comment is written, paradise doesn't convert trees 100% equal to what is written in source code
  // this flag tells tree converter to mimic paradise converter behaviour
  def paradiseCompatibilityHacks: Boolean = true

  // macro annotation itself must not appear in converted tree(paradise behaviour)
  protected val annotationToSkip: ScAnnotation
}


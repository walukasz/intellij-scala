package org.jetbrains.plugins.scala.findUsages.compilerReferences

import java.util
import java.util.concurrent.locks.{Lock, ReentrantLock}

import com.intellij.openapi.fileEditor.FileDocumentManager
import com.intellij.openapi.fileTypes.{FileType, FileTypeRegistry}
import com.intellij.openapi.module.Module
import com.intellij.openapi.project.Project
import com.intellij.openapi.roots.ProjectFileIndex
import com.intellij.openapi.util.UserDataHolderBase
import com.intellij.openapi.util.io.FileUtil
import com.intellij.openapi.vfs.newvfs.BulkFileListener
import com.intellij.openapi.vfs.newvfs.events._
import com.intellij.openapi.vfs.{VirtualFile, VirtualFileManager}
import com.intellij.psi.PsiDocumentManager
import com.intellij.psi.search.GlobalSearchScope
import com.intellij.psi.util.{CachedValueProvider, CachedValuesManager, PsiModificationTracker}
import com.intellij.util.containers.ContainerUtil
import org.jetbrains.annotations.Nullable
import org.jetbrains.plugins.scala.extensions._
import org.jetbrains.plugins.scala.project._

import scala.collection.JavaConverters._

/**
 * Mostly copy-pasted from [[com.intellij.compiler.backwardRefs.DirtyScopeHolder]], but modified to be able
 * to work with abstract [[Scope]]s (e.g. sbt project scoped to a particular configuration foo / Compile) instead of just IDEA modules.
  *
 * See also [[ScalaDirtyScopeHolder]].
 */
abstract class DirtyScopeHolder[Scope](
  project:        Project,
  fileTypes:      Array[FileType],
  fileIndex:      ProjectFileIndex,
  fileDocManager: FileDocumentManager,
  psiDocManager:  PsiDocumentManager
) extends UserDataHolderBase
    with BulkFileListener {

  protected val lock: Lock                                       = new ReentrantLock()
  protected val fileTypeRegistry: FileTypeRegistry               = FileTypeRegistry.getInstance()
  protected val vfsChangedScopes: util.Set[Scope]                = ContainerUtil.set[Scope]()
  protected val modifiedDuringIndexing: util.HashMap[Scope, Int] = ContainerUtil.newHashMap[Scope, Int]()
  protected val compilationAffectedScopes: util.Set[Scope]       = ContainerUtil.newConcurrentSet[Scope]()
  protected var indexingPhases: Int                              = 0

  protected def scopeForSourceContentFile(vfile: VirtualFile): Option[Scope]
  protected def moduleScopes(m: Module): Set[Scope]
  protected def scopeToSearchScope(scope: Scope): GlobalSearchScope

  private[compilerReferences] def markScopeUpToDate(scope: Scope): Unit = compilationAffectedScopes.add(scope)

  private[compilerReferences] def markProjectAsOutdated(): Unit = project.sourceModules.foreach(markModuleAsDirty)

  private[compilerReferences] def reset(): Unit = lock.locked {
    markProjectAsOutdated()
    modifiedDuringIndexing.clear()
    compilationAffectedScopes.clear()
    indexingPhases = 0
  }

  override def after(events: util.List[_ <: VFileEvent]): Unit = events.forEach {
    case e @ (_: VFileCreateEvent | _: VFileMoveEvent | _: VFileCopyEvent) => onFileChange(e.getFile)
    case pce: VFilePropertyChangeEvent =>
      val propertyName = pce.getPropertyName
      if (propertyName == VirtualFile.PROP_NAME || propertyName == VirtualFile.PROP_SYMLINK_TARGET)
        onFileChange(pce.getFile)
    case _ => ()
  }

  override def before(events: util.List[_ <: VFileEvent]): Unit = events.forEach {
    case e @ (_: VFileDeleteEvent | _: VFileMoveEvent | _: VFileContentChangeEvent) => onFileChange(e.getFile)
    case pce: VFilePropertyChangeEvent =>
      val propertyName = pce.getPropertyName
      if (propertyName == VirtualFile.PROP_NAME || propertyName == VirtualFile.PROP_SYMLINK_TARGET) {
        val path = pce.getPath

        project.sourceModules.foreach(
          m => if (FileUtil.isAncestor(path, m.getModuleFilePath, true)) markModuleAsDirty(m)
        )
      }
    case _ => ()
  }

  private[this] def onFileChange(@Nullable vfile: VirtualFile): Unit =
    for {
      file  <- vfile.toOption
      scope <- scopeForSourceContentFile(file)
    } addToDirtyScopes(scope)

  protected def markModuleAsDirty(m: Module): Unit = lock.locked(moduleScopes(m).foreach(addToDirtyScopes))

  protected def addToDirtyScopes(scope: Scope): Unit = lock.locked(
    if (indexingPhases != 0) modifiedDuringIndexing.merge(scope, indexingPhases, Math.max(_, _))
    else                     vfsChangedScopes.add(scope)
  )

  private[compilerReferences] def indexingStarted(): Unit = lock.locked(indexingPhases += 1)

  private[compilerReferences] def indexingFinished(): Unit = lock.locked {
    indexingPhases -= 1
    vfsChangedScopes.removeAll(compilationAffectedScopes)

    val iter = modifiedDuringIndexing.entrySet().iterator()
    while (iter.hasNext) {
      val entry = iter.next()
      entry.setValue(entry.getValue - indexingPhases)

      if (entry.getValue == 0) iter.remove()
      else                     addToDirtyScopes(entry.getKey)
    }
  }

  private[compilerReferences] def installVFSListener(): Unit =
    project.getMessageBus.connect(project).subscribe(VirtualFileManager.VFS_CHANGES, this)

  def dirtyScope: GlobalSearchScope = inReadAction {
    lock.locked {
      if (indexingPhases != 0) GlobalSearchScope.allScope(project)
      else if (!project.isDisposed) {
        CachedValuesManager
          .getManager(project)
          .getCachedValue(
            this,
            () =>
              CachedValueProvider.Result
                .create(calcDirtyScope(), PsiModificationTracker.MODIFICATION_COUNT, VirtualFileManager.getInstance())
          )
      } else GlobalSearchScope.EMPTY_SCOPE
    }
  }

  private[this] def calcDirtyScope(): GlobalSearchScope =
    GlobalSearchScope.union(dirtyScopes.map(scopeToSearchScope)(collection.breakOut))

  def contains(file: VirtualFile): Boolean = dirtyScope.contains(file)

  def dirtyScopes: Set[Scope] = {
    val dirty = Set.newBuilder[Scope]
    dirty ++= vfsChangedScopes.asScala

    fileDocManager.getUnsavedDocuments.foreach { doc =>
      for {
        file  <- fileDocManager.getFile(doc).toOption
        scope <- scopeForSourceContentFile(file)
      } dirty += scope
    }

    psiDocManager.getUncommittedDocuments.foreach { doc =>
      for {
        pfile <- psiDocManager.getPsiFile(doc).toOption
        vfile <- pfile.getVirtualFile.toOption
        scope <- scopeForSourceContentFile(vfile)
      } dirty += scope
    }

    dirty.result()
  }
}
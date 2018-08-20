package org.jetbrains.jps.incremental.scala
package local

import java.io.File
import java.util.Optional
import java.util.jar.JarFile

import scala.util.Try

import org.jetbrains.jps.incremental.scala.data.CompilationData
import org.jetbrains.jps.incremental.scala.local.zinc.Utils._
import org.jetbrains.jps.incremental.scala.local.zinc.{BinaryToSource, _}
import org.jetbrains.jps.incremental.scala.model.CompileOrder
import sbt.internal.inc._
import xsbti.compile._

/**
  * @author Pavel Fatin
  */
class SbtCompiler(javaTools: JavaTools, optScalac: Option[ScalaCompiler], fileToStore: File => AnalysisStore) extends AbstractCompiler {
  def compile(compilationData: CompilationData, client: Client): Unit = optScalac match {
    case Some(scalac) => doCompile(compilationData, client, scalac)
    case _ =>
      client.error(s"No scalac found to compile scala sources: ${compilationData.sources.take(10).mkString("\n")}...")
  }

  private def doCompile(compilationData: CompilationData, client: Client, scalac: ScalaCompiler): Unit = {
    client.progress("Loading cached results...")

    val incrementalCompiler = new IncrementalCompilerImpl

    val order = compilationData.order match {
      case CompileOrder.Mixed => xsbti.compile.CompileOrder.Mixed
      case CompileOrder.JavaThenScala => xsbti.compile.CompileOrder.JavaThenScala
      case CompileOrder.ScalaThenJava => xsbti.compile.CompileOrder.ScalaThenJava
    }

    val analysisStore = fileToStore(compilationData.cacheFile)
    val zincMetadata = CompilationMetadata.load(analysisStore, client, compilationData, scalac.scalaInstance().version())
    import zincMetadata._

    client.progress("Searching for changed files...")

    val progress = getProgress(client)
    val reporter = getReporter(client)
    val logger = getLogger(client, zincLogFilter)

    val intellijLookup = IntellijExternalLookup(compilationData, client, cacheDetails.isCached)
    val intellijClassfileManager = new IntellijClassfileManager

    DefinesClassCache.invalidateCacheIfRequired(compilationData.zincData.compilationStartDate)

    val incOptions = IncOptions.of()
      .withExternalHooks(IntelljExternalHooks(intellijLookup, intellijClassfileManager))
      .withStoreApis(false)
      .withRecompileOnMacroDef(Optional.of(false))
      .withTransitiveStep(5) // Default 3 was not enough for us
      .withIgnoredScalacOptions(compilationData.zincData.ignoredScalacOptions.toArray)

    val cs = incrementalCompiler.compilers(javaTools, scalac)
    val setup = incrementalCompiler.setup(
      IntellijEntryLookup(compilationData, fileToStore),
      /*skip = */ false,
      compilationData.cacheFile,
      CompilerCache.fresh,
      incOptions,
      reporter,
      Option(progress),
      Array.empty)
    val previousResult = PreviousResult.create(Optional.of(previousAnalysis), previousSetup.toOptional)

    val finalOutput = if (compilationData.zincData.isToJar) {
      new File(compilationData.output.getParentFile, compilationData.output.getName + ".jar")
    } else compilationData.output

    val inputs = incrementalCompiler.inputs(
      compilationData.classpath.toArray,
      compilationData.zincData.allSources.toArray,
      finalOutput,
      compilationData.scalaOptions.toArray,
      compilationData.javaOptions.toArray,
      100,
      Array(),
      order,
      cs,
      setup,
      previousResult)

    val compilationResult = Try {
      client.progress("Collecting incremental compiler data...")
      val result: CompileResult = incrementalCompiler.compile(inputs, logger)

      if (result.hasModified || cacheDetails.isCached) {
        analysisStore.set(AnalysisContents.create(result.analysis(), result.setup()))

        intellijClassfileManager.deletedDuringCompilation().foreach(_.foreach(client.deleted))

        val binaryToSource = BinaryToSource(result.analysis, compilationData)

        client.startProcessingOutput(finalOutput)

        val importedBinaries = if (cacheDetails.isCached) previousAnalysis.asInstanceOf[Analysis].stamps.allProducts else Nil
        val generatedClassFiles = intellijClassfileManager.generatedDuringCompilation().flatten
        val allClassFiles = importedBinaries ++ generatedClassFiles

        def sourceForBinary(binary: File): Option[File] =
          binaryToSource.classfileToSources(binary).headOption

        def addClassNames(classFiles: Iterable[File]): Seq[(File, String)] =
          classFiles.map(cf => (cf, binaryToSource.className(cf))).toVector

        val allGenerated = allClassFiles
          .groupBy(sourceForBinary)
          .collect { case (Some(source), classFiles) => source -> addClassNames(classFiles) }
          .toVector

        client.allGenerated(allGenerated)

        client.stopProcessingOutput(finalOutput)
      }
      result
    }

    compilationResult.recover {
      case e: CompileFailed =>
        // The error should be already handled via the `reporter`
        // However we need to invalidate source from last compilation
        val sourcesForInvalidation: Iterable[File] =
          if (intellijClassfileManager.deletedDuringCompilation().isEmpty) compilationData.sources
          else BinaryToSource(previousAnalysis, compilationData)
            .classfilesToSources(intellijClassfileManager.deletedDuringCompilation().last) ++ compilationData.sources

        sourcesForInvalidation.foreach(source => client.sourceStarted(source.getAbsolutePath))
      case e: Throwable =>
        // Invalidate analysis
        previousSetup.foreach(previous => analysisStore.set( AnalysisContents.create(Analysis.empty, previous)))

        // Keep files dirty
        compilationData.zincData.allSources.foreach(source => client.sourceStarted(source.getAbsolutePath))

        val msg =
          s"""Compilation faild when compiling to: ${compilationData.output}
              |  ${e.getMessage}
              |  ${e.getStackTrace.mkString("\n  ")}
          """.stripMargin

        client.error(msg, None, None, None)
    }

    zincMetadata.compilationFinished(compilationData, compilationResult, intellijClassfileManager, cacheDetails)
  }
}
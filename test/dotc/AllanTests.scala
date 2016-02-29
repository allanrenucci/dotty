package dotc

import java.io.File

import org.junit.Test

import test._

class AllanTests extends CompilerTest {

  override val defaultOutputDir: String = "./out/"

  val dottyDir  = "./dotty-annotated/src/dotty/"

  val noCheckOptions = List(
//    "-verbose",
//    "-Ylog:frontend",
//    "-Xprompt",
//    "-explaintypes",
//    "-Yshow-suppressed-errors",
    "-pagewidth",
    "160")

  implicit val defaultOptions = noCheckOptions ++ List(
    "-Yno-double-bindings",
    "-d",
    defaultOutputDir
  )

  private def deleteFilesInFolder(folder: File, deleteFolder: Boolean = false): Unit = {
    val files = folder.listFiles()

    files foreach { file =>
      if (file.isDirectory)
        deleteFilesInFolder(file, deleteFolder = true)
      else
        file.delete()
    }

    if (deleteFolder)
      folder.delete()
  }

  private def dotty() = compileDir(dottyDir, ".", List("-deep", "-Ycheck-reentrant", "-strict"))

  @Test def build(): Unit = {
    println("------------  Building dotty  ------------")
    // Empty output dir
    deleteFilesInFolder(new File(defaultOutputDir))
    // Build dotty
    dotty()
    // Make jar
    val p = Runtime.getRuntime.exec(Array("jar", "cf", "dotty.jar", "-C", "out", "."))
    p.waitFor()
  }
}

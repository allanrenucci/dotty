package dotc

import org.junit.Test

import test._

class AllanTests extends CompilerTest {

  override val defaultOutputDir: String = "./out/"

  val testsDir           = "./tests/"
  val autoCollectionsDir = testsDir + "autocollections/"
  //val runDir   = testsDir + "run/"
  //val posDir   = testsDir + "pos/"

  val noCheckOptions = List(
    //    "-verbose",
    //    "-Ylog:frontend",
    //    "-Xprompt",
    //    "-explaintypes",
    //    "-Yshow-suppressed-errors",
    "-pagewidth",
    "160")

  implicit val defaultOptions = noCheckOptions ++ List(
    "-Yno-deep-subtypes",
    "-Yno-double-bindings",
    "-d",
    defaultOutputDir,
    //"-Xprint:preauto,auto",
    "-Ycheck:auto"
  )

  //@Test def experiment = compileFile(autoCollectionsDir, "Experiment")
  //@Test def mergeSort  = compileFile(autoCollectionsDir, "MergeSort")
  @Test def quickSort  = compileFile(autoCollectionsDir, "QuickSort") // WrappedArray
  //@Test def bfs  = compileFile(autoCollectionsDir, "BFS") // Queue
  //@Test def dfs  = compileFile(autoCollectionsDir, "BFS") // Queue
  //@Test def pageRank = compileFile(autoCollectionsDir, "PageRank")
}

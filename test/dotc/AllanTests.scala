package dotc

import org.junit.Test

import test._

class AllanTests extends CompilerTest {

  override val defaultOutputDir: String = "./out/"

  val testsDir = "./tests/"
  val allandir = testsDir + "allan/"
  val runDir   = testsDir + "run/"
  val posDir   = testsDir + "pos/"

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
    "-Xprint:preauto,auto",
    "-Ycheck:auto"
  )

  @Test def allan = compileFile(allandir, "Experiment")
}

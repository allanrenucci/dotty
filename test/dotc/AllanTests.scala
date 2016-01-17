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
    "-Ycheck:idempotentCall"
    //"-Ycheck:tailrec,resolveSuper,idempotentCall,mixin,restoreScopes,labelDef"
  )

  //@Test def allan = compileFile(allandir, "Experiment")
  //@Test def allan = compileFile(allandir, "Test")
  //@Test def pos = compileFile(posDir, "idempotentcalls")
  //@Test def idempotent = runFile(runDir, "idempotentcalls")
  @Test def runAll = runFiles(runDir)

  //@Test def fixme1 = runFile(runDir, "t4859") // Initialisation order
  //@Test def idemSource = compileFile("./src/dotty/tools/dotc/transform/", "IdempotentCall")
}

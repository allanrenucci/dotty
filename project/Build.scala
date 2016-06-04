import sbt.Keys._
import sbt._
import java.io.{ RandomAccessFile, File }
import java.nio.channels.FileLock
import scala.reflect.io.Path

import org.scalajs.sbtplugin.ScalaJSPlugin
import org.scalajs.sbtplugin.ScalaJSPlugin.autoImport._

object DottyBuild extends Build {

  val jenkinsMemLimit = List("-Xmx1300m")

  val JENKINS_BUILD = "dotty.jenkins.build"

  val agentOptions = List(
    // "-agentlib:jdwp=transport=dt_socket,server=y,suspend=y,address=5005"
    // "-agentpath:/home/dark/opt/yjp-2013-build-13072/bin/linux-x86-64/libyjpagent.so"
    // "-agentpath:/Applications/YourKit_Java_Profiler_2015_build_15052.app/Contents/Resources/bin/mac/libyjpagent.jnilib",
    // "-XX:+HeapDumpOnOutOfMemoryError", "-Xmx1g", "-Xss2m"
  )

  override def settings: Seq[Setting[_]] = {
    super.settings ++ Seq(
      scalaVersion in Global := "2.11.5",
      version in Global := "0.1-SNAPSHOT",
      organization in Global := "org.scala-lang",
      organizationName in Global := "LAMP/EPFL",
      organizationHomepage in Global := Some(url("http://lamp.epfl.ch")),
      homepage in Global := Some(url("https://github.com/lampepfl/dotty")),

      // scalac options
      scalacOptions in Global ++= Seq("-feature", "-deprecation", "-language:existentials,higherKinds,implicitConversions"),

      javacOptions in Global ++= Seq("-Xlint:unchecked", "-Xlint:deprecation")
    )
  }

  /** Enforce 2.11.5. Do not let it be upgraded by dependencies. */
  private val overrideScalaVersionSetting =
    ivyScala := ivyScala.value.map(_.copy(overrideScalaVersion = true))

  lazy val `dotty-interfaces` = project.in(file("interfaces")).
    settings(
      // Do not append Scala versions to the generated artifacts
      crossPaths := false,
      // Do not depend on the Scala library
      autoScalaLibrary := false,
      //Remove javac invalid options in Compile doc
      javacOptions in (Compile, doc) --= Seq("-Xlint:unchecked", "-Xlint:deprecation")
    )

  lazy val dotty = project.in(file(".")).
    dependsOn(`dotty-interfaces`).
    settings(
      overrideScalaVersionSetting,

      // set sources to src/, tests to test/ and resources to resources/
      scalaSource in Compile := baseDirectory.value / "src",
      javaSource in Compile := baseDirectory.value / "src",
      scalaSource in Test := baseDirectory.value / "test",
      javaSource in Test := baseDirectory.value / "test",
      resourceDirectory in Compile := baseDirectory.value / "resources",
      unmanagedSourceDirectories in Compile := Seq((scalaSource in Compile).value),
      unmanagedSourceDirectories in Test := Seq((scalaSource in Test).value),

      // Generate compiler.properties, used by sbt
      resourceGenerators in Compile += Def.task {
        val file = (resourceManaged in Compile).value / "compiler.properties"
        val contents = s"version.number=${version.value}"
        IO.write(file, contents)
        Seq(file)
      }.taskValue,

      // include sources in eclipse (downloads source code for all dependencies)
      //http://stackoverflow.com/questions/10472840/how-to-attach-sources-to-sbt-managed-dependencies-in-scala-ide#answer-11683728
      com.typesafe.sbteclipse.plugin.EclipsePlugin.EclipseKeys.withSource := true,

      // get libraries onboard
      partestDeps := Seq("me.d-d" % "scala-compiler" % "2.11.5-20151022-113908-7fb0e653fd",
                         "org.scala-lang" % "scala-reflect" % scalaVersion.value,
                         "org.scala-lang" % "scala-library" % scalaVersion.value % "test"),
      libraryDependencies ++= partestDeps.value,
      libraryDependencies ++= Seq("org.scala-lang.modules" %% "scala-xml" % "1.0.1",
                                  "org.scala-lang.modules" %% "scala-partest" % "1.0.11" % "test",
                                  "com.novocode" % "junit-interface" % "0.11" % "test",
                                  "jline" % "jline" % "2.12"),

      // enable improved incremental compilation algorithm
      incOptions := incOptions.value.withNameHashing(true),

      // enable verbose exception messages for JUnit
      testOptions in Test += Tests.Argument(TestFrameworks.JUnit, "-a", "-v", "--run-listener=test.ContextEscapeDetector"),
      testOptions in Test += Tests.Cleanup({ () => partestLockFile.delete }),

      lockPartestFile := {
        // When this file is present, running `test` generates the files for
        // partest. Otherwise it just executes the tests directly.
        val lockDir = partestLockFile.getParentFile
        lockDir.mkdirs
        // Cannot have concurrent partests as they write to the same directory.
        if (lockDir.list.size > 0)
          throw new RuntimeException("ERROR: sbt partest: another partest is already running, pid in lock file: " + lockDir.list.toList.mkString(" "))
        partestLockFile.createNewFile
        partestLockFile.deleteOnExit
      },
      runPartestRunner <<= Def.inputTaskDyn {
        // Magic! This is both an input task and a dynamic task. Apparently
        // command line arguments get passed to the last task in an aliased
        // sequence (see partest alias below), so this works.
        val args = Def.spaceDelimited("<arg>").parsed
        val jars = Seq((packageBin in Compile).value.getAbsolutePath) ++
            getJarPaths(partestDeps.value, ivyPaths.value.ivyHome)
        val dottyJars  = "-dottyJars " + (jars.length + 1) + " dotty.jar" + " " + jars.mkString(" ")
        // Provide the jars required on the classpath of run tests
        runTask(Test, "dotty.partest.DPConsoleRunner", dottyJars + " " + args.mkString(" "))
      },

      /* Add the sources of scalajs-ir.
       * To guarantee that dotty can bootstrap without depending on a version
       * of scalajs-ir built with a different Scala compiler, we add its
       * sources instead of depending on the binaries.
       */
      ivyConfigurations += config("sourcedeps").hide,
      libraryDependencies +=
        "org.scala-js" %% "scalajs-ir" % scalaJSVersion % "sourcedeps",
      sourceGenerators in Compile += Def.task {
        val s = streams.value
        val cacheDir = s.cacheDirectory
        val trgDir = (sourceManaged in Compile).value / "scalajs-ir-src"

        val report = updateClassifiers.value
        val scalaJSIRSourcesJar = report.select(
            configuration = Set("sourcedeps"),
            module = (_: ModuleID).name.startsWith("scalajs-ir_"),
            artifact = artifactFilter(`type` = "src")).headOption.getOrElse {
          sys.error(s"Could not fetch scalajs-ir sources")
        }

        FileFunction.cached(cacheDir / s"fetchScalaJSIRSource",
            FilesInfo.lastModified, FilesInfo.exists) { dependencies =>
          s.log.info(s"Unpacking scalajs-ir sources to $trgDir...")
          if (trgDir.exists)
            IO.delete(trgDir)
          IO.createDirectory(trgDir)
          IO.unzip(scalaJSIRSourcesJar, trgDir)
          (trgDir ** "*.scala").get.toSet
        } (Set(scalaJSIRSourcesJar)).toSeq
      }.taskValue,

      // Adjust classpath for running dotty
      mainClass in (Compile, run) := Some("dotty.tools.dotc.Main"),
      fork in run := true,
      fork in Test := true,
      parallelExecution in Test := false,

      // http://grokbase.com/t/gg/simple-build-tool/135ke5y90p/sbt-setting-jvm-boot-paramaters-for-scala
      javaOptions <++= (dependencyClasspath in Runtime, packageBin in Compile) map { (attList, bin) =>
        // put the Scala {library, reflect} in the classpath
        val path = for {
          file <- attList.map(_.data)
          path = file.getAbsolutePath
        } yield "-Xbootclasspath/p:" + path
        // dotty itself needs to be in the bootclasspath
        val fullpath = ("-Xbootclasspath/a:" + bin) :: path.toList
        // System.err.println("BOOTPATH: " + fullpath)

        val travis_build = // propagate if this is a travis build
          if (sys.props.isDefinedAt(JENKINS_BUILD))
            List(s"-D$JENKINS_BUILD=${sys.props(JENKINS_BUILD)}") ::: jenkinsMemLimit
          else
            List()

        val tuning =
          if (sys.props.isDefinedAt("Oshort"))
            // Optimize for short-running applications, see https://github.com/lampepfl/dotty/issues/222
            List("-XX:+TieredCompilation", "-XX:TieredStopAtLevel=1")
          else
            List()

        ("-DpartestParentID=" + pid) :: tuning ::: agentOptions ::: travis_build ::: fullpath
      }
    ).
    settings(
      addCommandAlias("partest",                   ";test:package;package;test:runMain dotc.build;lockPartestFile;test:test;runPartestRunner") ++
      addCommandAlias("partest-only",              ";test:package;package;test:runMain dotc.build;lockPartestFile;test:test-only dotc.tests;runPartestRunner") ++
      addCommandAlias("partest-only-no-bootstrap", ";test:package;package;                        lockPartestFile;test:test-only dotc.tests;runPartestRunner") ++
      addCommandAlias("test-allan",                ";test:package;package;                        lockPartestFile;test:test-only dotc.AllanTests;runPartestRunner") ++
      addCommandAlias("test-allan-bootstrap",      ";test:package;package;test:runMain dotc.build;lockPartestFile;test:test-only dotc.AllanTests;runPartestRunner")
    )

  /** A sandbox to play with the Scala.js back-end of dotty.
   *
   *  This sandbox is compiled with dotty with support for Scala.js. It can be
   *  used like any regular Scala.js project. In particular, `fastOptJS` will
   *  produce a .js file, and `run` will run the JavaScript code with a JS VM.
   *
   *  Simply running `dotty/run -scalajs` without this sandbox is not very
   *  useful, as that would not provide the linker and JS runners.
   */
  lazy val sjsSandbox = project.in(file("sandbox/scalajs")).
    enablePlugins(ScalaJSPlugin).
    settings(
      overrideScalaVersionSetting,

      /* Remove the Scala.js compiler plugin for scalac, and enable the
       * Scala.js back-end of dotty instead.
       */
      libraryDependencies ~= { deps =>
        deps.filterNot(_.name.startsWith("scalajs-compiler"))
      },
      scalacOptions += "-scalajs",

      // The main class cannot be found automatically due to the empty inc.Analysis
      mainClass in Compile := Some("hello.world"),

      // While developing the Scala.js back-end, it is very useful to see the trees dotc gives us
      scalacOptions += "-Xprint:labelDef",

      /* Debug-friendly Scala.js optimizer options.
       * In particular, typecheck the Scala.js IR found on the classpath.
       */
      scalaJSOptimizerOptions ~= {
        _.withCheckScalaJSIR(true).withParallel(false)
      }
    ).
    settings(compileWithDottySettings).
    settings(inConfig(Compile)(Seq(
      /* Make sure jsDependencyManifest runs after compile, otherwise compile
       * might remove the entire directory afterwards.
       */
      jsDependencyManifest <<= jsDependencyManifest.dependsOn(compile)
    )))

  lazy val `dotty-bench` = project.in(file("bench")).
    dependsOn(dotty % "compile->test").
    settings(
      overrideScalaVersionSetting,

      baseDirectory in (Test,run) := (baseDirectory in dotty).value,

      libraryDependencies ++= Seq("com.storm-enroute" %% "scalameter" % "0.6" % Test,
        "com.novocode" % "junit-interface" % "0.11"),
      testFrameworks += new TestFramework("org.scalameter.ScalaMeterFramework"),

      fork in Test := true,
      parallelExecution in Test := false,

      // http://grokbase.com/t/gg/simple-build-tool/135ke5y90p/sbt-setting-jvm-boot-paramaters-for-scala
      javaOptions <++= (dependencyClasspath in Runtime, packageBin in Compile) map { (attList, bin) =>
        // put the Scala {library, reflect, compiler} in the classpath
        val path = for {
          file <- attList.map(_.data)
          path = file.getAbsolutePath
          prefix = if (path.endsWith(".jar")) "p" else "a"
        } yield "-Xbootclasspath/" + prefix + ":" + path
        // dotty itself needs to be in the bootclasspath
        val fullpath = ("-Xbootclasspath/a:" + bin) :: path.toList
        // System.err.println("BOOTPATH: " + fullpath)

        val travis_build = // propagate if this is a travis build
          if (sys.props.isDefinedAt(JENKINS_BUILD))
            List(s"-D$JENKINS_BUILD=${sys.props(JENKINS_BUILD)}")
          else
            List()
        val res = agentOptions ::: travis_build ::: fullpath
        println("Running with javaOptions: " + res)
        res
      }
    )

  // Partest tasks
  lazy val lockPartestFile = TaskKey[Unit]("lockPartestFile", "Creates the lock file at ./tests/locks/partest-<pid>.lock")
  lazy val partestLockFile = new File("." + File.separator + "tests" + File.separator + "locks" + File.separator + s"partest-$pid.lock")
  def pid = java.lang.Long.parseLong(java.lang.management.ManagementFactory.getRuntimeMXBean().getName().split("@")(0))

  lazy val runPartestRunner = InputKey[Unit]("runPartestRunner", "Runs partest")

  lazy val partestDeps = SettingKey[Seq[ModuleID]]("partestDeps", "Finds jars for partest dependencies")
  def getJarPaths(modules: Seq[ModuleID], ivyHome: Option[File]): Seq[String] = ivyHome match {
    case Some(home) =>
      modules.map({ module =>
        val file = Path(home) / Path("cache") /
          Path(module.organization) / Path(module.name) / Path("jars") /
          Path(module.name + "-" + module.revision + ".jar")
        if (!file.isFile) throw new RuntimeException("ERROR: sbt getJarPaths: dependency jar not found: " + file)
        else file.jfile.getAbsolutePath
      })
    case None => throw new RuntimeException("ERROR: sbt getJarPaths: ivyHome not defined")
  }

  // Compile with dotty
  lazy val compileWithDottySettings = {
    inConfig(Compile)(inTask(compile)(Defaults.runnerTask) ++ Seq(
      // Compile with dotty
      fork in compile := true,

      compile := {
        val inputs = (compileInputs in compile).value
        import inputs.config._

        val s = streams.value
        val logger = s.log
        val cacheDir = s.cacheDirectory

        // Discover classpaths

        def cpToString(cp: Seq[File]) =
          cp.map(_.getAbsolutePath).mkString(java.io.File.pathSeparator)

        val compilerCp = Attributed.data((fullClasspath in (dotty, Compile)).value)
        val cpStr = cpToString(classpath ++ compilerCp)

        // List all my dependencies (recompile if any of these changes)

        val allMyDependencies = classpath filterNot (_ == classesDirectory) flatMap { cpFile =>
          if (cpFile.isDirectory) (cpFile ** "*.class").get
          else Seq(cpFile)
        }

        // Compile

        val cachedCompile = FileFunction.cached(cacheDir / "compile",
            FilesInfo.lastModified, FilesInfo.exists) { dependencies =>

          logger.info(
              "Compiling %d Scala sources to %s..." format (
              sources.size, classesDirectory))

          if (classesDirectory.exists)
            IO.delete(classesDirectory)
          IO.createDirectory(classesDirectory)

          val sourcesArgs = sources.map(_.getAbsolutePath()).toList

          /* run.run() below in doCompile() will emit a call to its
           * logger.info("Running dotty.tools.dotc.Main [...]")
           * which we do not want to see. We use this patched logger to
           * filter out that particular message.
           */
          val patchedLogger = new Logger {
            def log(level: Level.Value, message: => String) = {
              val msg = message
              if (level != Level.Info ||
                  !msg.startsWith("Running dotty.tools.dotc.Main"))
                logger.log(level, msg)
            }
            def success(message: => String) = logger.success(message)
            def trace(t: => Throwable) = logger.trace(t)
          }

          def doCompile(sourcesArgs: List[String]): Unit = {
            val run = (runner in compile).value
            run.run("dotty.tools.dotc.Main", compilerCp,
                "-classpath" :: cpStr ::
                "-d" :: classesDirectory.getAbsolutePath() ::
                options ++:
                sourcesArgs,
                patchedLogger) foreach sys.error
          }

          // Work around the Windows limitation on command line length.
          val isWindows =
            System.getProperty("os.name").toLowerCase().indexOf("win") >= 0
          if ((fork in compile).value && isWindows &&
              (sourcesArgs.map(_.length).sum > 1536)) {
            IO.withTemporaryFile("sourcesargs", ".txt") { sourceListFile =>
              IO.writeLines(sourceListFile, sourcesArgs)
              doCompile(List("@"+sourceListFile.getAbsolutePath()))
            }
          } else {
            doCompile(sourcesArgs)
          }

          // Output is all files in classesDirectory
          (classesDirectory ** AllPassFilter).get.toSet
        }

        cachedCompile((sources ++ allMyDependencies).toSet)

        // We do not have dependency analysis when compiling externally
        sbt.inc.Analysis.Empty
      }
    ))
  }
}

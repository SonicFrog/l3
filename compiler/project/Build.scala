import sbt._
import Keys._
import Process._

/**
 *  The main sbt configuration for the compiler build.
 */
object L3Build extends Build {

  val defaults = Defaults.defaultSettings ++ Seq(
    // scala compiler version:
    scalaVersion := "2.11.7",
    scalaBinaryVersion := "2.11",
    scalacOptions ++= Seq("-feature", "-deprecation", "-unchecked"),

    // source location configuration:
    scalaSource in Compile <<= baseDirectory(_ / "src"),
    scalaSource in Test <<= baseDirectory(_ / "test"),
    resourceDirectory in Compile <<= baseDirectory(_ / "resources"),

    // Library dependencies (not including those needed for tests)
    libraryDependencies += "com.lihaoyi" %% "fastparse" % "0.3.4",

    // eclipse configuration:
    unmanagedSourceDirectories in Compile <<= (scalaSource in Compile)(Seq(_)),
    unmanagedSourceDirectories in Test <<= (scalaSource in Test)(Seq(_)),
    com.typesafe.sbteclipse.plugin.EclipsePlugin.EclipseKeys.withSource := true,

    // run configuration
    fork in run := true,
    connectInput in run := true,
    outputStrategy := Some(StdoutOutput),
    javaOptions in run ++= Seq("-Xss32M", "-Xms128M"),

    // test configuration:
    libraryDependencies += "com.novocode" % "junit-interface" % "0.10-M2" % "test",
    testOptions += Tests.Argument(TestFrameworks.JUnit, "-q", "-v"),
    parallelExecution in Test := false,
    // fork in test
    fork in Test := true,
    javaOptions in Test <++= baseDirectory map {
      base => Seq("-Djava.security.manager", "-Djava.security.policy=" + base + "/project/tests.policy") },
    javaOptions in Test ++= Seq("-Xss32M", "-Xms128M"),

    // packaging configuration (also include tests, create submit-me.jar):
    unmanagedSources in (Compile, packageSrc) <++= unmanagedSources in Test,
    mappings in (Compile, packageSrc) <<=
      (unmanagedSources in (Compile, packageSrc), baseDirectory) map { (srcs, base) =>
        srcs x Path.relativeTo(base)
      },
    artifactPath in (Compile, packageSrc) <<= submitMe,
    cleanFiles <+= submitMe
  )

  // The location desired for the source jar that will be submitted
  def submitMe = baseDirectory(_ / "submit-me.jar")

  lazy val l3 = Project(id = "l3", base = file("."), settings = defaults)
}

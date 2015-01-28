import sbt._
import Keys._
import scala.scalajs.sbtplugin.ScalaJSPlugin._
import ScalaJSKeys._
import com.typesafe.sbt.SbtStartScript
import sbtassembly.Plugin._
import AssemblyKeys._
import spray.revolver.RevolverPlugin._
import com.lihaoyi.workbench.Plugin._
object Build extends sbt.Build{

  lazy val root = project.in(file("."))
    .aggregate(client, page, server, runtime)
    .settings(assemblySettings:_*)
    .settings(
      bootSnippet := "",
      SbtStartScript.stage in Compile := Unit,
      (assembly in Compile) := {
        (SbtStartScript.stage in Compile).value
        (assembly in (server, Compile)).value
      },
      (resources in (server, Compile)) ++= {
        (fastOptJS in (client, Compile)).value
        (managedClasspath in (runtime, Compile)).value.map(_.data) ++ Seq(
          (packageBin in (runtime, Compile)).value,
          (packageBin in (page, Compile)).value,
          (packageBin in (shared, Compile)).value,
          (artifactPath in (client, Compile, fastOptJS)).value
        )
      },
      scalaVersion := "2.11.1"
    )
  lazy val shared = project.in(file("shared")).settings(scalaJSSettings:_*)
                           .settings(scalaVersion := "2.11.1")

  lazy val client = project
    .dependsOn(page, shared)
    .settings(scalaJSSettings:_*)
    .settings(
      libraryDependencies ++= Seq(
        "org.scala-lang.modules.scalajs" %%% "scalajs-dom" % "0.6",
        "com.scalatags" %%% "scalatags" % "0.3.8",
        "com.scalarx" %%% "scalarx" % "0.2.5",
        "com.lihaoyi" %%% "upickle" % "0.1.4",
        "com.lihaoyi" %%% "autowire" % "0.1.0",
        "org.scala-lang.modules" %% "scala-async" % "0.9.1" % "provided",
        "com.lihaoyi" %% "acyclic" % "0.1.2" % "provided",
        "com.lihaoyi" %%% "autowire" % "0.1.0"
      ),
      (SbtStartScript.stage in Compile) := (fullOptJS in Compile).value,
      relativeSourceMaps := true,
      addCompilerPlugin("com.lihaoyi" %% "acyclic" % "0.1.2"),
      autoCompilerPlugins := true,
      scalaVersion := "2.11.1"
    )
  lazy val page = project
    .dependsOn(shared)
    .settings(scalaJSSettings:_*)
    .settings(
      libraryDependencies ++= Seq(
        "org.scala-lang.modules.scalajs" %%% "scalajs-dom" % "0.6",
        "com.scalatags" %%% "scalatags" % "0.3.8"
      ),
      scalaVersion := "2.11.1"
    )
  lazy val runtime = project
    .dependsOn(page)
    .settings(scalaJSSettings:_*)
    .settings(
      resolvers += Resolver.sonatypeRepo("snapshots"),
      libraryDependencies ++= Seq(
        "org.scala-lang" % "scala-reflect" % scalaVersion.value,
        "org.scala-lang.modules.scalajs" %%% "scalajs-dom" % "0.6",
        "com.scalatags" %%% "scalatags" % "0.3.8",
        "org.scala-lang.modules" %% "scala-async" % "0.9.1" % "provided",
        "com.scalarx" %%% "scalarx" % "0.2.5",
        "com.nativelibs4java" %% "scalaxy-loops" % "0.1.1" % "provided"
      ),
      autoCompilerPlugins := true,
      scalaVersion := "2.11.1"
    )

  lazy val server = project
    .dependsOn(shared)
    .settings(assemblySettings ++ Revolver.settings ++ SbtStartScript.startScriptForClassesSettings:_*)
    .settings(
      resolvers += Resolver.url("scala-js-releases",
        url("http://dl.bintray.com/content/scala-js/scala-js-releases"))(
          Resolver.ivyStylePatterns),
      libraryDependencies ++= Seq(
        "org.scala-lang" % "scala-compiler" % scalaVersion.value,
        "com.typesafe.akka" %% "akka-actor" % "2.3.2",
        "io.spray" %% "spray-can" % "1.3.1",
        "io.spray" %% "spray-client" % "1.3.1",
        "io.spray" %% "spray-caching" % "1.3.1",
        "io.spray" %% "spray-httpx" % "1.3.1",
        "io.spray" %% "spray-routing" % "1.3.1",
        "io.spray" %% "spray-json" % "1.3.1",
        "org.scala-lang.modules.scalajs" % s"scalajs-compiler_${scalaVersion.value}" % "0.5.0",
        "org.scala-lang.modules.scalajs" %% "scalajs-tools" % "0.5.0",
        "org.scala-lang.modules" %% "scala-async" % "0.9.1" % "provided",
        "com.scalatags" %% "scalatags" % "0.3.8",
        "com.lihaoyi" %% "acyclic" % "0.1.2" % "provided",
        "org.webjars" % "ace" % "01.08.2014",
        "org.webjars" % "jquery" % "2.1.0-2",
        "org.webjars" % "normalize.css" % "2.1.3",
        "com.lihaoyi" %% "upickle" % "0.1.4",
        "com.lihaoyi" %% "autowire" % "0.1.0",
        "com.lihaoyi" %% "utest" % "0.1.6" % "test",
        "org.mozilla" % "rhino" % "1.7R4" % "test"
      ),

      resolvers += "Typesafe Repo" at "http://repo.typesafe.com/typesafe/releases/",
      testFrameworks += new TestFramework("utest.runner.JvmFramework"),
      javaOptions += "-XX:MaxPermSize=2g",
      javaOptions in Revolver.reStart += "-Xmx2g",
      addCompilerPlugin("com.lihaoyi" %% "acyclic" % "0.1.2"),
      autoCompilerPlugins := true,
      scalaVersion := "2.11.1"
    )
}

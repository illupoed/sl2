import sbt._
import Keys._

object MyBuild extends Build {

  val paradiseVersion = "2.0.0-M3"

  lazy val root: Project = Project(
    "root",
    file("."),
    settings = Project.defaultSettings ++ Seq(
      resolvers += Resolver.sonatypeRepo("snapshots"),
	  resolvers += Resolver.sonatypeRepo("releases"),
      addCompilerPlugin("org.scalamacros" % "paradise" % paradiseVersion cross CrossVersion.full)
    ) ++ Seq(
      libraryDependencies <+= (scalaVersion)("org.scala-lang" % "scala-reflect" % _),
      libraryDependencies ++= (
        if (scalaVersion.value.startsWith("2.10")) List("org.scalamacros" % "quasiquotes" % paradiseVersion cross CrossVersion.full)
        else Nil
      )
    )
  )
}

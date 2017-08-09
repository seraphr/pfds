import Dependencies._
import com.typesafe.sbt.SbtScalariform.ScalariformKeys
import scalariform.formatter.preferences.{AlignSingleLineCaseStatements, DoubleIndentClassDeclaration}

val RiformSettings = (
  ScalariformKeys.preferences := (ScalariformKeys.preferences.value
    .setPreference(AlignSingleLineCaseStatements, true)
    .setPreference(AlignSingleLineCaseStatements.MaxArrowIndent, 60))
    .setPreference(DoubleIndentClassDeclaration, false))


lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "jp.seraphr",
      scalaVersion := "2.12.2",
      version      := "0.1.0-SNAPSHOT"
    )),
    name := "pfds",
    libraryDependencies += scalaTest % Test,
    RiformSettings
  )

name := "fpinscala"

ThisBuild / scalaVersion := "3.3.4"

ThisBuild / githubWorkflowJavaVersions := Seq(JavaSpec.temurin("23"))

ThisBuild / githubWorkflowBuild := Seq(WorkflowStep.Sbt(name = Some("Build project"), commands = List("test:compile")))

ThisBuild / scalacOptions ++= List("-feature", "-deprecation", "-Ykind-projector:underscores", "-source:future")

ThisBuild / libraryDependencies += "org.scalameta" %% "munit" % "1.1.0" % Test

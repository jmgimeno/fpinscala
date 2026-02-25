name := "fpinscala"

ThisBuild / scalaVersion := "3.8.2"

ThisBuild / githubWorkflowBuild := Seq(WorkflowStep.Sbt(name = Some("Build project"), commands = List("test:compile")))

ThisBuild / githubWorkflowJavaVersions := Seq(JavaSpec.temurin("17"))

ThisBuild / scalacOptions ++= List("-feature", "-deprecation", "-Xkind-projector:underscores", "-source:future")

ThisBuild / libraryDependencies += "org.scalameta" %% "munit" % "1.2.2" % Test

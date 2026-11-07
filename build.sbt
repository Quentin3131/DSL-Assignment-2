val scala3Version = "3.9.0"

lazy val root = project
  .in(file("."))
  .settings(
    organization := "ch.usi.si.maise.edsl",
    developers := List(Developer("firstname.lastname", "FirstName LastName", "firstanme.lastname@usi.ch", url("https://yourwebsite.com"))),
    name := "assignment-02-template",
    version := "2026.1.0",
    scalaVersion := scala3Version,
    libraryDependencies += "org.parboiled" %% "parboiled" % "2.5.1",
    libraryDependencies += "net.sourceforge.plantuml" % "plantuml" % "1.2023.12",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.19" % Test
  )

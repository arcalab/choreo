name := "choreo"

version := "0.1"

scalaVersion := "2.12.10"

scalacOptions += "-Ypartial-unification"

libraryDependencies ++= Seq(
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2",
  "org.typelevel" %% "cats-core" % "2.1.1"
//  "org.typelevel" %% "cats-core" % "2.0.0"//,
  //"org.typelevel" %% "cats-effect" % "2.0.0"
)

addCompilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.1")
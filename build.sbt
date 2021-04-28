enablePlugins(ScalaJSPlugin)

//scalaJSUseMainModuleInitializer := true

//mainClass := Some("choreo.frontend.widgets.Main.main")

name := "choreo"

version := "0.1"

//scalaVersion := "2.12.10"
scalaVersion := "3.0.0-M1"

//scalacOptions ++= {
//  if (isDotty.value) Seq("-source:3.0-migration")
//  else Seq.empty
//}

scalacOptions ++= Seq("-indent") // "-indent" // Seq("-rewrite" ,"-new-syntax")

// scalacOptions += "-Ypartial-unification"

libraryDependencies ++= Seq(
  ("org.scala-lang.modules" %% "scala-parser-combinators" %  "1.1.2").withDottyCompat(scalaVersion.value),
  ("org.typelevel" %% "cats-core" % "2.1.1").withDottyCompat(scalaVersion.value),
  /////
  ("be.doeraene" %%% "scalajs-jquery" % "1.0.0").withDottyCompat(scalaVersion.value), //"0.9.1",
  /////
  ("org.scala-js" %%% "scalajs-dom" % "1.1.0").withDottyCompat(scalaVersion.value), //"0.9.1",
  ("com.lihaoyi" %%% "scalatags" % "0.9.1").withDottyCompat(scalaVersion.value), //"0.6.7",

//  "org.typelevel" %% "cats-core" % "2.0.0"//,
  //"org.typelevel" %% "cats-effect" % "2.0.0"
  // "org.scalatest" % "scalatest_3.0.0-M1" % "3.2.3" % Test
)

//addCompilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.1")


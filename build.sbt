val scala3Version = "3.1.1" // "3.0.0-RC3"

lazy val caos = project.in(file("lib/caos"))
  .enablePlugins(ScalaJSPlugin)
  .settings(scalaVersion := scala3Version)

lazy val choreo = project.in(file("."))
  .enablePlugins(ScalaJSPlugin)
  .settings(
    name := "ceta",
    version := "0.1.0",
    scalaVersion := scala3Version,
    scalacOptions += "-new-syntax",
    scalaJSUseMainModuleInitializer := true,
    Compile / mainClass := Some("choreo.frontend.Main"),
    //Compile / fastLinkJS / artifactPath := baseDirectory.value / "lib" / "caos" / "tool" / "js" / "gen" / "fastLink.js",
    Compile / fastLinkJS / scalaJSLinkerOutputDirectory := baseDirectory.value / "lib" / "caos"/ "tool" / "js" / "gen",
    Compile / fullLinkJS / scalaJSLinkerOutputDirectory := baseDirectory.value / "lib" / "caos"/ "tool" / "js" / "gen",
    //Compile / fastLinkJS / scalaJSLinkerConfig ~= (_.withOutputPatterns(OutputPatterns.fromJSFile(name.value))),
    libraryDependencies ++= Seq(
      "org.scala-lang.modules" %%% "scala-parser-combinators" %  "2.1.0",
      "org.typelevel" %%% "cats-core" % "2.6.1",
      ("be.doeraene" %%% "scalajs-jquery" % "1.0.0").cross(CrossVersion.for3Use2_13),//.withDottyCompat(scalaVersion.value),
      ("org.scala-js" %%% "scalajs-dom" % "1.2.0").cross(CrossVersion.for3Use2_13),//.withDottyCompat(scalaVersion.value),
      ("com.lihaoyi" %%% "scalatags" % "0.9.1").cross(CrossVersion.for3Use2_13)//.withDottyCompat(scalaVersion.value)
    )
  )
  .dependsOn(caos)


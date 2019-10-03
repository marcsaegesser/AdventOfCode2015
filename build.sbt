lazy val advent2015 = (project in file("."))
  .settings (
    name := "Advent2015",
    organization := "org.saegesser",
    version := "0.0.0-SNAPSHOT",
    scalaVersion in ThisBuild := "2.13.1",
    scalacOptions in ThisBuild ++= Seq(
      "-feature",
      "-deprecation",
      // "-Yno-adapted-args",
      "-Ywarn-value-discard",
      "-Ywarn-numeric-widen",
      "-Ywarn-dead-code",
      "-Xlint",
      "-Xfatal-warnings",
      "-unchecked",
      "-language:implicitConversions"
    ),

    scalacOptions in (Compile, console) ~= (_.filterNot(_ == "-Xlint")),
    scalacOptions in (Test, console) ~= (_.filterNot(_ == "-Xlint")),

    libraryDependencies ++= Seq(
      "org.scalatest"    %% "scalatest"     % "3.0.8"   % "test",
      "org.scalacheck"   %% "scalacheck"    % "1.14.1"  % "test"
    ),

    initialCommands := "import advent._"
  )

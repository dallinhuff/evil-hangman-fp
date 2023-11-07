ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.3.1"

lazy val root = (project in file("."))
  .settings(
    name := "EvilHangman",
    idePackagePrefix := Some("com.dallinhuff.evilhangman"),
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-effect"    % "3.5.0",
      "com.monovore"  %% "decline"        % "2.4.1",
      "com.monovore"  %% "decline-effect" % "2.4.1"
    ),
    assembly / assemblyJarName := "evil-hangman.jar"
  )


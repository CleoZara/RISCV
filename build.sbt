ThisBuild / scalaVersion := "2.13.10"
ThisBuild / version      := "0.1.0"

lazy val root = (project in file("."))
  .settings(
    name := "riscv-pipeline",
    libraryDependencies ++= Seq(
      "edu.berkeley.cs" %% "chisel3"    % "3.5.6",
      "edu.berkeley.cs" %% "chiseltest" % "0.5.4" % Test,
      "org.scalatest"   %% "scalatest"  % "3.2.17" % Test,
    ),
    scalacOptions ++= Seq(
      "-language:reflectiveCalls",
      "-deprecation",
      "-feature",
      "-Xcheckinit",
    ),
    Compile / unmanagedSourceDirectories := Seq(baseDirectory.value / "src" / "main"),
    Compile / unmanagedSources := (Compile / unmanagedSources).value.filterNot { f =>
      val path = f.getPath.replace('\\', '/')
      path.endsWith("src/main/Icache/ICacheMissFSM.scala") ||
      path.endsWith("/PrefetchSpec.scala")
    },
    Test / unmanagedSourceDirectories := Seq(baseDirectory.value / "src" / "test"),
    Test / unmanagedSources := (Test / unmanagedSources).value.filterNot { f =>
      val path = f.getPath.replace('\\', '/')
      path.endsWith("/PrefetchSpec.scala") && !path.contains("/src/test/")
    },
    addCompilerPlugin(
      "edu.berkeley.cs" % "chisel3-plugin" % "3.5.6" cross CrossVersion.full
    ),
    Test / fork := true,
  )

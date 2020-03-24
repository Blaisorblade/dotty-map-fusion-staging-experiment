val dottyVersion = "0.23.0-RC1"

lazy val root = project
  .in(file("."))
  .settings(
    name := "dotty-simple",
    version := "0.1.0",

    scalaVersion := dottyVersion,

    libraryDependencies ++= Seq(
      "ch.epfl.lamp" %% "dotty-staging" % dottyVersion,
      "com.novocode" % "junit-interface" % "0.11" % "test"
    ),
    scalacOptions ++= Seq("-Yshow-suppressed-errors")
  )

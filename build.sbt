name := "point"

//libraryDependencies += "org.specs2" %% "specs2-core" % "3.8.8" % "test"

scalacOptions in Test ++= Seq("-Yrangepos")

//libraryDependencies += "com.badlogicgames.gdx" % "gdx" % "1.9.5"

organization := "com.example"
version := "1.0"
scalaVersion := "2.13.0-pre-SNAPSHOT"

libraryDependencies += "org.specs2" % "specs2-core_2.13.0-M5" % "4.3.6"

updateOptions := updateOptions.value.withLatestSnapshots(false)

fork in run := true

name := "point"

//libraryDependencies += "org.specs2" %% "specs2-core" % "3.8.8" % "test"

scalacOptions in Test ++= Seq("-Yrangepos")

//libraryDependencies += "com.badlogicgames.gdx" % "gdx" % "1.9.5"

organization := "com.example"
version := "1.0"
scalaVersion := "2.13.1"

libraryDependencies += "org.specs2" %% "specs2-core" % "4.8.1" % Test

updateOptions := updateOptions.value.withLatestSnapshots(false)

fork in run := true

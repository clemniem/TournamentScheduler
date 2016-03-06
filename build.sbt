name := "tournament-scheduler"

version := "1.0"

scalaVersion := "2.11.7"
enablePlugins(JavaAppPackaging)

libraryDependencies += "org.scalatest"  %% "scalatest"   % "2.2.4" % Test
libraryDependencies += "com.typesafe.akka" % "akka-testkit_2.11" % "2.4.1"
libraryDependencies += "com.typesafe.akka" % "akka-actor_2.11" % "2.4.1"
libraryDependencies += "com.typesafe.akka" % "akka-http-experimental_2.11" % "2.4.2"
libraryDependencies += "com.typesafe.akka" % "akka-http-core-experimental_2.11" % "2.0.3"
libraryDependencies += "com.typesafe.akka" % "akka-stream-experimental_2.11" % "2.0.3"
libraryDependencies += "com.typesafe.akka" % "akka-http-spray-json-experimental_2.11" % "2.4.2"
libraryDependencies += "io.spray" %%  "spray-json" % "1.3.2"
libraryDependencies += "com.github.nscala-time" %% "nscala-time" % "2.10.0"

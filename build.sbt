name := """life"""
organization := "com.dotgoing"

version := "1.0-SNAPSHOT"

lazy val root = (project in file(".")).enablePlugins(PlayScala)

scalaVersion := "2.12.3"

libraryDependencies += guice
libraryDependencies += jdbc
libraryDependencies += evolutions

libraryDependencies += "org.scalatestplus.play" %% "scalatestplus-play" % "3.1.2" % Test
libraryDependencies += "com.h2database" % "h2" % "1.4.192"
libraryDependencies += "org.webjars" % "jquery" % "2.1.3"
libraryDependencies += "com.jsuereth" % "scala-arm_2.12" % "2.0"


// Adds additional packages into Twirl
//TwirlKeys.templateImports += "com.dotgoing.controllers._"

// Adds additional packages into conf/routes
// play.sbt.routes.RoutesKeys.routesImport += "com.dotgoing.binders._"

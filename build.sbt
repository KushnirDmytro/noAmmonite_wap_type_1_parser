name := "noAmmonite_wap_type_1_parser"

version := "0.1"

scalaVersion := "2.12.3"

libraryDependencies += "com.lihaoyi" %% "fastparse" % "0.4.4"

libraryDependencies += "io.circe" % "circe-core_2.12" % "0.8.0"

libraryDependencies += "io.circe" %% "circe-generic" % "0.8.0"


resolvers += Resolver.sonatypeRepo("releases")
addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)
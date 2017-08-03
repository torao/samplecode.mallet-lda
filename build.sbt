name := "mallet-sample-lda"

organization := "at.hazm"

version := "1.0"

scalaVersion := "2.12.3"

resolvers ++= Seq(
  "CodeLibs Repository" at "http://maven.codelibs.org/"
)

libraryDependencies ++= Seq(
  "cc.mallet" % "mallet" % "2.0.8",
  "org.codelibs" % "elasticsearch-analysis-kuromoji-neologd" % "5.5.0"
)
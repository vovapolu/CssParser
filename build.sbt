import sbt.Keys._

name := "CssParser"

version := "1.0"

scalaVersion := "2.11.8"

libraryDependencies += "com.lihaoyi" % "fastparse_2.11" % "0.3.7"
libraryDependencies += "com.lihaoyi" % "utest_2.11" % "0.4.3"

testFrameworks += new TestFramework("utest.runner.Framework")
    
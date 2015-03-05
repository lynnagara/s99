scalaVersion := "2.11.4"

name := "99 problems"

libraryDependencies += "com.lihaoyi" %% "utest" % "0.3.0"

testFrameworks += new TestFramework("utest.runner.Framework")
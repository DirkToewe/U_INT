name := "U_INT"

version := "0.1"

scalaVersion := "2.12.8"

libraryDependencies ++= Seq(
  "com.lihaoyi" %% "utest" % "0.6.6" % "test",
  "com.chuusai" %% "shapeless" % "2.3.3"
)

testFrameworks += new TestFramework("utest.runner.Framework")

scalacOptions ++= Seq(
  "-feature"
//  "-Xlog-implicits"
)

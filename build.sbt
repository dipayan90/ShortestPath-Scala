name := "uw-scala"

organization := "com.persist"

version := "1.0.0"

scalaVersion := "2.11.8"

viewSettings

scalacOptions ++= Seq("-deprecation", "-unchecked", "-feature")

scalacOptions ++= Seq("-Yrangepos")

testFrameworks += new TestFramework("com.fortysevendeg.lambdatest.sbtinterface.LambdaFramework")

libraryDependencies ++= Seq(
  "org.scala-lang" % "scala-reflect" % scalaVersion.value,
  "com.persist" % "persist-json_2.11" % "1.1.3",
  "com.fortysevendeg" % "lambda-test_2.11" % "1.1.2" % "test"
)



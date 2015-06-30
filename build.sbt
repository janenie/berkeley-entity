import AssemblyKeys._ // put this at the top of the file

name := "berkeley-entity"

version := "1"

scalaVersion := "2.11.7"

assemblySettings

mainClass in assembly := Some("edu.berkeley.nlp.entity.Driver")

unmanagedResourceDirectories in Compile += { baseDirectory.value / "resources/" }

scalacOptions ++= Seq("-optimise")//, "-Yinline-warnings", "-feature", "-deprecation")


libraryDependencies += (
    "org.deeplearning4j" % "deeplearning4j-nlp" % "0.0.3.3.4.alpha2"
      exclude("javax.jms", "jms")
      exclude("com.sun.jdmk", "jmxtools")
      exclude("com.sun.jmx", "jmxri")
      //exclude("org.deeplearning4j","deeplearning4j-scaleout-akka")
      exclude("org.deeplearning4j", "deeplearning4j-scaleout-zookeeper")
      excludeAll(ExclusionRule(organization = "org.spark-project.akka"),
      ExclusionRule(organization = "com.typesafe.akka"))
  )



//exclude("org.spark-project.akka", "akka-actor_2.10")
//exclude("org.spark-project.akka", "akka-remote_2.10")
//exclude("org.spark-project.akka", "akka-slf4j_2.10")

libraryDependencies ++= Seq(
  "com.typesafe.akka" % "akka-actor_2.11" % "2.3.4",
  "com.typesafe.akka" % "akka-cluster_2.11" % "2.3.4",
  "com.typesafe.akka" % "akka-contrib_2.11" % "2.3.4",
  "com.typesafe.akka" % "akka-persistence-experimental_2.11"% "2.3.4",
  "com.typesafe.akka" % "akka-remote_2.11" % "2.3.4",
  "com.typesafe.akka" % "akka-slf4j_2.11" % "2.3.4"
)

//libraryDependencies ++= Seq(
//  "org.spark-project.akka" % "akka-actor_2.11" % "2.3.4-spark",
//  "org.spark-project.akka" % "akka-remote_2.11" % "2.3.4-spark",
//  "org.spark-project.akka" % "akka-slf4j_2.11" % "2.3.4-spark"
//)


libraryDependencies += (
  "org.nd4j" % "nd4j-jblas" % "0.0.3.5.5.4"
  )

libraryDependencies += (
  // match version brought in by deeplearning4j
  "org.apache.commons" % "commons-compress" % "1.8.1"
  )

mergeStrategy in assembly := {
  //case x if Assembly.isConfigFile(x) => MergeStrategy.concat
  case "META-INF/MANIFEST.MF" => MergeStrategy.discard
  case "reference.conf" => MergeStrategy.concat
  case s: String => MergeStrategy.first
}

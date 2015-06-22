import AssemblyKeys._ // put this at the top of the file

name := "berkeley-entity"

version := "1"

scalaVersion := "2.11.6"

assemblySettings

mainClass in assembly := Some("edu.berkeley.nlp.entity.Driver")

unmanagedResourceDirectories in Compile += { baseDirectory.value / "resources/" }

scalacOptions ++= Seq("-optimise")//, "-Yinline-warnings", "-feature", "-deprecation")

libraryDependencies += (
    "org.deeplearning4j" % "deeplearning4j-nlp" % "0.0.3.3.4.alpha2"
      exclude("javax.jms", "jms")
      exclude("com.sun.jdmk", "jmxtools")
      exclude("com.sun.jmx", "jmxri")
      exclude("org.deeplearning4j","deeplearning4j-scaleout-akka")
      exclude("org.deeplearning4j", "deeplearning4j-scaleout-zookeeper")
      exclude("org.spark-project.akka", "akka-actor")
      excludeAll(ExclusionRule(organization = "org.springframework"))
  )

mergeStrategy in assembly := {
  //case x if Assembly.isConfigFile(x) => MergeStrategy.concat
  case "META-INF/MANIFEST.MF" => MergeStrategy.discard
  case s: String => MergeStrategy.first
}
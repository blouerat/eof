scalaVersion := "2.12.4"

libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.2.19"

enablePlugins(TutPlugin)

tutTargetDirectory := baseDirectory.value / "public"

scalacOptions ++= Seq(
  "-deprecation",
  "-feature",
  "-language:higherKinds",
  "-Xfatal-warnings"
)

name := "medit"

version := "0.1"

lazy val `main` = project.in(file("src-main")).settings(
  sharedSettings,
  libraryDependencies ++= {
    val version = "3.2.3"
    Seq(
      "lwjgl",
      "lwjgl-glfw",
      "lwjgl-opengl"
      //Add other modules here
    ).flatMap {
      module => {
        Seq(
          "org.lwjgl" % module % version,
          "org.lwjgl" % module % version classifier "natives-windows" //Change to linux/mac
        )
      }
    } ++ Seq(
      "org.bytedeco" % "skia-platform" % "1.68.0-1.5.2"
    )
  },
  fork in run := true,
  baseDirectory in run := file("."),
)

val sharedSettings = Seq(
  scalaVersion := "2.13.1",
  resolvers ++= Seq(
//    "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/",
//    Resolver.jcenterRepo,
//    Resolver.sonatypeRepo("releases"),
    // "jitpack" at "https://jitpack.io" // this is for the custom built parser combinator library
  ),
  sources in (Compile, doc) := Seq.empty,
  publishArtifact in (Compile, packageDoc) := false,
  javacOptions ++= Seq(
    "-Xdiags:verbose"
  ),
  scalacOptions ++= Seq(
    "-language:implicitConversions",
    "-deprecation", // Emit warning and location for usages of deprecated APIs.
    "-feature", // Emit warning and location for usages of features that should be imported explicitly.
    "-unchecked", // Enable additional warnings where generated code depends on assumptions.
    //"-P:acyclic:force",
  ),
  //  autoCompilerPlugins := true,
  //  addCompilerPlugin("com.lihaoyi" %% "acyclic" %  "0.2.0"),
  //  libraryDependencies += "com.lihaoyi" %% "acyclic" % "0.2.0" % "provided",
)


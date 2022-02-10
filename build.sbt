import sbtcrossproject.CrossPlugin.autoImport.{crossProject, CrossType}



name := "medit"

version := "0.1"

lazy val `main` = crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Pure)
    .in(file("src-main")).settings(
  sharedSettings,
  libraryDependencies ++= Seq(
  )
).jvmSettings(
  libraryDependencies ++= Seq(
    "com.lihaoyi" %%% "upickle" % "0.9.8"
  )
)

lazy val `parser-parser-tree-sitter` = project.in(file("src-parser-tree-sitter")).settings(
  sharedSettings,
  libraryDependencies ++= Seq(
    "com.github.JetBrains" % "jsitter" % "-SNAPSHOT"
  )
).dependsOn(`main`.jvm)

lazy val `gui-jvm` = project.in(file("src-gui-jvm")).settings(
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
          "org.lwjgl" % module % version classifier "natives-macos" //windows/linux/mac
        )
      }
    } ++ Seq(
      "org.bytedeco" % "skia-platform" % "2.80.3-1.5.7"
    )
  },
).dependsOn(`main`.jvm, `parser-parser-tree-sitter`)
    .enablePlugins(JavaAppPackaging)
    .enablePlugins(GraalVMNativeImagePlugin)


val sharedSettings = Seq(
  scalaVersion := "2.13.1",
  resolvers ++= Seq(
    "jitpack" at "https://jitpack.io"
    //    "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/",
//    Resolver.jcenterRepo,
//    Resolver.sonatypeRepo("releases"),
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
  fork in run := true,
  baseDirectory in run := file("."),
  //  autoCompilerPlugins := true,
  //  addCompilerPlugin("com.lihaoyi" %% "acyclic" %  "0.2.0"),
  //  libraryDependencies += "com.lihaoyi" %% "acyclic" % "0.2.0" % "provided",
)

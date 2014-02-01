name := "shapeless-rules"

version := "1.0-SNAPSHOT"

libraryDependencies ++= Seq(
  "com.chuusai" % "shapeless" % "2.0.0-M1" cross CrossVersion.full,
  "org.scalaz" %% "scalaz-core" % "7.0.5"
)

resolvers ++= Seq(
  Resolver.file("local-play", file("/Users/pvo/zenexity/workspaces/workspace_tools/jto/Play20/repository/local"))(Resolver.ivyStylePatterns),
  "Sonatype OSS Releases"  at "http://oss.sonatype.org/content/repositories/releases/",
  "Sonatype OSS Snapshots" at "http://oss.sonatype.org/content/repositories/snapshots/"
)


//scalacOptions ++= Seq("-unchecked", "-deprecation", "-Xlog-implicits")

play.Project.playScalaSettings

import sbt._
import Keys._

object SkeletonBuild extends Build {

    val sharedSettings = Project.defaultSettings ++ Seq(
        organization        := "eu.balamut",
        version             := "1",
        scalaVersion        := "2.11.1",

        crossScalaVersions := Seq(scalaVersion.value, "2.10.4"),

        libraryDependencies ++= Seq(
            "com.chuusai" %% "shapeless" % "2.0.0",
            "org.scalaz" %% "scalaz-core" % "7.0.6",
            "org.scalatest" %% "scalatest" % "2.1.6" % "test"
        ),

        libraryDependencies <+= (scalaVersion)("org.scala-lang" % "scala-reflect" % _),

        incOptions := incOptions.value.withNameHashing(true),

        resolvers       ++= Seq(
            "snapshots" at "http://oss.sonatype.org/content/repositories/snapshots",
            "releases"  at "http://oss.sonatype.org/content/repositories/releases",
            "Concurrent Maven Repo" at "http://conjars.org/repo"
        ),

        scalacOptions   ++= Seq(
              "-g:vars",
              "-unchecked", "-deprecation",
              "-encoding", "UTF8",
              "-feature",
              "-language:implicitConversions", "-language:postfixOps",
              "-Xfatal-warnings",
              "-target:jvm-1.7"),

        javacOptions    ++= Seq(
              "-g", "-encoding", "UTF8",
              "-Xlint:all", "-Xlint:-serial", "-Xlint:-path",
              "-Werror",
              "-Xlint:-options", "-source", "7", "-target", "7"),

        publishMavenStyle := true,

        publishArtifact in Test := true,
        publishArtifact in packageDoc := false,
        publishArtifact in (Test, packageDoc) := false,

        pomIncludeRepository := { x => false },

        pomExtra := (
            <url>https://github.com/lbalamut/sbt-skeleton</url>
            <scm>
                <url>git@github.com:lbalamut/sbt-skeleton.git</url>
                <connection>scm:git:git@github.com:lbalamut/sbt-skeleton.git</connection>
            </scm>
            <developers>
                <developer>
                    <id>lbalamut</id>
                    <name>Lukasz Balamut</name>
                    <url>http://twitter.com/lbalamut</url>
                </developer>
            </developers>)
    )

    lazy val core = Project(
        id = "root",
        base = file("."),
        settings = sharedSettings
    )
}

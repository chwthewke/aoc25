import sbt._
import sbt.Keys._

ThisBuild / organization := "net.chwthewke"

ThisBuild / ideBasePackages.withRank( KeyRanks.Invisible ) := Seq( "net.chwthewke.aoc" )

ThisBuild / Compile / doc / sources                := Seq.empty
ThisBuild / Compile / packageDoc / publishArtifact := false

enablePlugins( Scalafmt )
enablePlugins( Dependencies )

val sharedSettings = Seq(
  scalaVersion                                          := "3.7.4",
  ideExcludedDirectories.withRank( KeyRanks.Invisible ) := Seq( target.value )
)

val aggregateSettings = Seq(
  publish      := {},
  publishLocal := {}
)

val `advent-of-code-core` = project
  .in( file( "core" ) )
  .settings( sharedSettings )
  .settings( cats, catsEffect )
  .enablePlugins( Scalac )

val `advent-of-code-app` = project
  .in( file( "app" ) )
  .settings( sharedSettings )
  .settings( Compile / run / fork := true )
  .dependsOn( `advent-of-code-core` )
  .enablePlugins( Scalac, BuildInfo )

val `advent-of-code-tests` = project
  .in( file( "tests" ) )
  .settings( sharedSettings )
  .settings( scalatest, scalacheck )
  .dependsOn( `advent-of-code-core`, `advent-of-code-app` )
  .enablePlugins( Scalac )

val `advent-of-code` = project
  .in( file( "." ) )
  .settings( sharedSettings, aggregateSettings )
  .aggregate(
    `advent-of-code-core`,
    `advent-of-code-app`,
    `advent-of-code-tests`
  )

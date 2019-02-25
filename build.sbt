import sbtcrossproject.CrossPlugin.autoImport.{CrossType, crossProject}
import ReleaseTransformations._
import sbt.Keys.name

import scala.language.postfixOps
import sbt.{file, _}

lazy val username = "openlaw"
lazy val repo     = "openlaw-core"

licenses += ("Apache-2.0", url("https://opensource.org/licenses/Apache-2.0"))

lazy val scalaV = "2.12.8"
lazy val catsV = "1.6.0"
lazy val parboiledV = "2.1.5"
lazy val circeV = "0.11.1"
lazy val playJsonV = "2.7.1"

lazy val repositories = Seq(
  Resolver.jcenterRepo,
  "central" at "http://central.maven.org/maven2/",
  "scalaz-bintray" at "https://dl.bintray.com/scalaz/releases",
  "maven central" at "https://mvnrepository.com/repos/central",
  Resolver.mavenLocal
)

scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature")

lazy val commonSettings = Seq(
  organization := "org.openlaw",
  name := "openlaw-core",
  scalaVersion := scalaV,
  wartremoverErrors ++= rules
)

lazy val publishSettings = Seq(
  homepage := Some(url(s"https://github.com/$username/$repo")),
  licenses += ("Apache-2.0", url("https://opensource.org/licenses/Apache-2.0")),
  bintrayReleaseOnPublish in ThisBuild := true,
  bintrayOrganization := Some("openlawos"),
  bintrayRepository := "openlaw-core",
  bintrayPackageLabels := Seq("openlaw-core"),
  scmInfo := Some(ScmInfo(url(s"https://github.com/$username/$repo"), s"git@github.com:$username/$repo.git")),
  releaseCrossBuild := true,
  developers := List(
    Developer(
      id = "adridadou",
      name = "David Roon",
      email = "david.roon@consensys.net",
      url = new URL(s"http://github.com/adridadou")
    ),
    Developer(
      id = "outkaj",
      name = "Jacqueline Outka",
      email = "jacqueline@outka.xyz",
      url = new URL(s"http://github.com/outkaj")
    ),
    Developer(
      id = "openlawbot",
      name = "Pizza Dog Bot",
      email = "felipe@openlaw.io",
      url = new URL(s"http://github.com/openlawbot")
    )
  ),
  publishTo in ThisBuild := Some("Bintray" at "https://api.bintray.com/maven/openlawos/openlaw-core/openlaw-core/;publish=1"),
)

lazy val releaseSettings = releaseProcess := Seq[ReleaseStep](
  checkSnapshotDependencies,              // : ReleaseStep
  inquireVersions,                        // : ReleaseStep
  //setReleaseVersion,                      // : ReleaseStep
  //commitReleaseVersion,                   // : ReleaseStep, performs the initial git checks
  //tagRelease,                             // : ReleaseStep
  //releaseStepCommandAndRemaining("publish"),
  publishArtifacts,                       // : ReleaseStep,
  //setNextVersion,                         // : ReleaseStep
  //commitNextVersion,                      // : ReleaseStep
  //pushChanges                             // : ReleaseStep, also checks that an upstream branch is properly configured
)

val rules = Seq(Wart.ArrayEquals, Wart.OptionPartial, Wart.EitherProjectionPartial, Wart.Enumeration, Wart.ExplicitImplicitTypes, Wart.FinalVal, Wart.JavaConversions, Wart.JavaSerializable, Wart.LeakingSealed)

lazy val openlawCore = crossProject(JSPlatform, JVMPlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(CrossType.Pure) // [Pure, Full, Dummy], default: CrossType.Full
  .in(file("shared"))
  .jvmSettings(
    libraryDependencies ++= Seq(
      "io.circe"                %% "circe-iteratee"      % "0.12.0",
      "io.iteratee"             %% "iteratee-monix"      % "0.18.0",
      "io.monix"                %% "monix-eval"          % "3.0.0-fbcb270",
      "io.monix"                %% "monix-execution"     % "3.0.0-fbcb270",
      "io.circe"                %% "circe-core"          % circeV,
      "io.circe"                %% "circe-generic"       % circeV,
      "io.circe"                %% "circe-parser"        % circeV,
      "com.typesafe.play"       %% "play-json"           % playJsonV,
      "org.parboiled"           %% "parboiled"           % parboiledV,
      "org.typelevel"           %% "cats-core"           % catsV,
      "org.typelevel"           %% "cats-free"           % catsV,
      "io.github.cquiroz"       %% "scala-java-time"     % "2.0.0-RC1",
      "biz.enef"                %% "slogging-slf4j"      % "0.6.1",
      "com.lihaoyi"             %% "scalatags"           % "0.6.7",
      //Test
      "org.scalacheck"          %% "scalacheck"          % "1.14.0"       % Test,
      "org.scalatest"           %% "scalatest"           % "3.2.0-SNAP10"  % Test,
    )
  ).jsSettings(
    libraryDependencies ++= Seq(
      "io.github.cquiroz"       %%% "scala-java-time"      % "2.0.0-RC1",
      "io.github.cquiroz"       %%% "scala-java-time-tzdb" % "2.0.0-RC1_2018f",
      "org.parboiled"           %%% "parboiled"            % parboiledV,
      "com.chuusai"             %%% "shapeless"            % "2.3.3",
      "biz.enef"                %%% "slogging"             % "0.6.1",
      "org.typelevel"           %%% "cats-core"            % catsV,
      "org.typelevel"           %%% "cats-free"            % catsV,
      "io.circe"                %%% "circe-core"           % circeV,
      "io.circe"                %%% "circe-generic"        % circeV,
      "io.circe"                %%% "circe-parser"         % circeV,
      "com.typesafe.play"       %%% "play-json"            % playJsonV,
      "com.lihaoyi"             %%% "scalatags"            % "0.6.7",
      //Test
      "org.scalacheck"          %%% "scalacheck"          % "1.14.0"        % Test,
      "org.scalatest"           %%% "scalatest"           % "3.2.0-SNAP10"  % Test
    )
  )
  .settings(parallelExecution in Test := false)
  .settings(commonSettings: _*)
  .settings(publishSettings: _*)
  .settings(releaseSettings: _*)
  .enablePlugins(WartRemover)

lazy val openlawCoreJvm = openlawCore.jvm
lazy val openlawCoreJs = openlawCore.js

val root = (project in file("."))
  .dependsOn(openlawCoreJvm, openlawCoreJs)
  .aggregate(openlawCoreJvm, openlawCoreJs)

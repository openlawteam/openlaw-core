import sbtcrossproject.CrossPlugin.autoImport.{CrossType, crossProject}
import sbtrelease.ReleasePlugin.autoImport.releaseCrossBuild
import ReleaseTransformations._
import sbt.Keys.{name, publishTo}

import scala.language.postfixOps
import sbt.{file, _}
import sbtghpackages.GitHubPackagesPlugin.autoImport.githubRepository

lazy val username = "openlaw"
lazy val repo = "openlaw-core"

licenses += ("Apache-2.0", url("https://opensource.org/licenses/Apache-2.0"))

/*
The Scala and SBT versions must be matched to the version of scala-builder used
as the base image of the container. We try to standardize across projects and
always upgrade in a controlled fashion.

If you wish to update either Scala or SBT, please open an issue and and tag
@openlawteam/infra.
 */
lazy val scalaV = "2.12.11"

lazy val scalaJavaTimeV = "2.0.0-RC3"
lazy val catsV = "1.6.1"
lazy val parboiledV = "2.1.8"
lazy val circeV = "0.11.1"
lazy val playJsonV = "2.7.4"
lazy val scalaTagsV = "0.7.0"
lazy val sLoggingV = "0.6.1"
lazy val enumeratumV = "1.5.13"
lazy val scalaCheckV = "1.14.0"
lazy val scalaTestV = "3.2.0-SNAP10"

lazy val repositories = Seq(
  "GitHub Packages" at "https://maven.pkg.github.com/openlawteam",
  Resolver.jcenterRepo,
  "central" at "https://repo1.maven.org/maven2/",
  "maven central" at "https://mvnrepository.com/repos/central",
  Resolver.mavenLocal
)

ThisBuild / publishTo := Some(
  "GitHub Packages OpenLaw Core" at "https://maven.pkg.github.com/openlawteam/openlaw-core"
)

lazy val commonSettings = Seq(
  organization := "org.openlaw",
  name := "openlaw-core",
  scalaVersion := scalaV,
  wartremoverErrors ++= rules,
  scalacOptions ++= Seq(
    "-unchecked",
    "-deprecation",
    "-feature",
    "-language:implicitConversions"
  ),
  javacOptions ++= Seq("-source", "1.8", "-target", "1.8"),
  publishMavenStyle := true,
  Test / parallelExecution := false,
  releaseCrossBuild := true,
  Test / packageBin / publishArtifact := false,
  Test / packageDoc / publishArtifact := false,
  githubOwner := "openlawteam",
  githubRepository := "openlaw-core",
  githubTokenSource := TokenSource.GitConfig("github.token") || TokenSource
    .Environment("GITHUB_TOKEN") || TokenSource
    .Environment("TOKEN")
)

lazy val releaseSettings = releaseProcess := Seq[ReleaseStep](
  checkSnapshotDependencies // : ReleaseStep
  //inquireVersions,                        // : ReleaseStep
  //setReleaseVersion,                      // : ReleaseStep
  //commitReleaseVersion,                   // : ReleaseStep, performs the initial git checks
  //tagRelease,                             // : ReleaseStep
  //releaseStepCommandAndRemaining("publish"),
  //publishArtifacts // : ReleaseStep,
  //setNextVersion,                         // : ReleaseStep
  //commitNextVersion,                      // : ReleaseStep
  //pushChanges                             // : ReleaseStep, also checks that an upstream branch is properly configured
)

val rules = Seq(
  Wart.AnyVal,
  Wart.ArrayEquals,
  Wart.AsInstanceOf,
  Wart.EitherProjectionPartial,
  Wart.Enumeration,
  Wart.ExplicitImplicitTypes,
  Wart.FinalCaseClass,
  Wart.FinalVal,
  Wart.IsInstanceOf,
  Wart.JavaConversions,
  Wart.JavaSerializable,
  Wart.LeakingSealed,
  Wart.OptionPartial
)

/*
      *** Library Dependencies. ***

      Note that we are actively trying to reduce the scope of our dependencies.

      If you are considering adding a new dependency to the application, please
      take the following steps:

        A.) Evaluate the new dependency thoroughly:

          1) Does the dependency solve the use case in a way that it justifies
          using it versus writing the code ourselves? What issues does it solve
          for us?

          2) Is it actively maintained? Are issues/PRs responded to in a timely
          fashion?

          3) Have you reviewed the dependencies' source code, checking that it
          handles things in a reasonable way? (Does it have appropriate error
          handling? Do you feel like we can understand and modify the code if we
          need to fork it? Would you allow this code to be merged into our code
          base if you saw it in a PR?).

          4) How is the dependency licensed? Is it compatible with our OSS
          requirements?

          5) What alternative packages or approaches did you consider?

        B.) Add the dependency below. You must now add a comment inline with the
        dependency explaining what the dependency is used for (for one example,
        so we know if we no longer need it in future!) Try to maintain
        rough alphabetical order overall (but grouping related deps is also
        helpful).

        C.) Include a detailed description in your PR of your findings for the
        above. You MUST explicitly answer all of the questions above!

        D.) Make sure you get code review sign-off from the appropriate parties.
        We maintain a list of dependency reviewers at `.github/CODEOWNERS`
        (their name, not ours), which should automatically add them to the PR
        when this file is modified.
 */

lazy val openlawCore = crossProject(JSPlatform, JVMPlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(CrossType.Pure) // the project does not have separate sources for JVM and JS
  .in(file("shared"))
  .jvmSettings(
    libraryDependencies ++= Seq(
      //circe is used to serialize / deserialize json
      "io.circe" %% "circe-core" % circeV,
      "io.circe" %% "circe-generic" % circeV,
      "io.circe" %% "circe-parser" % circeV,
      "io.circe" %% "circe-java8" % circeV,
      //parser / interpreter library. Used for our markup language
      "org.parboiled" %% "parboiled" % parboiledV,
      //cats is used for FP constructs
      "org.typelevel" %% "cats-core" % catsV,
      "org.typelevel" %% "cats-free" % catsV,
      // scala.js compatible library to use time types
      "io.github.cquiroz" %% "scala-java-time" % scalaJavaTimeV,
      // logging library that is compatible with scala.js
      "biz.enef" %% "slogging-slf4j" % sLoggingV,
      // enumeratum provides type-safe enumerations with improvements over stdlib enumerations
      "com.beachape" %% "enumeratum" % enumeratumV,
      // scalatags is used for composition of XHTML documents in the document printers
      "com.lihaoyi" %% "scalatags" % scalaTagsV,
      //Test
      "org.scalacheck" %% "scalacheck" % scalaCheckV % Test,
      "org.scalatest" %% "scalatest" % scalaTestV % Test,
      //Play json is used in tests to make it easier to prepare json in the tests. It shouldn't be used in the library
      "com.typesafe.play" %% "play-json" % playJsonV % Test
    )
  )
  .jsSettings(
    libraryDependencies ++= Seq(
      //circe is used to serialize / deserialize json
      "io.circe" %%% "circe-core" % circeV,
      "io.circe" %%% "circe-generic" % circeV,
      "io.circe" %%% "circe-parser" % circeV,
      "io.circe" %%% "circe-java8" % circeV,
      //parser / interpreter library. Used for our markup language
      "org.parboiled" %%% "parboiled" % parboiledV,
      //cats is used for FP constructs
      "org.typelevel" %%% "cats-core" % catsV,
      "org.typelevel" %%% "cats-free" % catsV,
      // scala.js compatible library to use time types
      "io.github.cquiroz" %%% "scala-java-time" % scalaJavaTimeV,
      // timezone handling is in a separate library because of its size and not everybody needs it. we do
      "io.github.cquiroz" %%% "scala-java-time-tzdb" % "2.0.0-RC3_2019a",
      // logging library that is compatible with scala.js
      "biz.enef" %%% "slogging" % sLoggingV,
      // enumeratum provides type-safe enumerations with improvements over stdlib enumerations
      "com.beachape" %%% "enumeratum" % enumeratumV,
      // scalatags is used for composition of XHTML documents in the document printers
      "com.lihaoyi" %%% "scalatags" % scalaTagsV,
      //Test
      "org.scalacheck" %%% "scalacheck" % scalaCheckV % Test,
      "org.scalatest" %%% "scalatest" % scalaTestV % Test,
      //Play json is used in tests to make it easier to prepare json in the tests. It shouldn't be used in the library
      "com.typesafe.play" %%% "play-json" % playJsonV % Test
    )
  )
  .settings(commonSettings: _*)
  .settings(releaseSettings: _*)
  .enablePlugins(WartRemover)

lazy val version = git.gitDescribedVersion

// need commands for releasing both Scala & ScalaJS to avoid issue where release-with-defaults only releases Scala lib

lazy val openlawCoreJvm = openlawCore.jvm
lazy val openlawCoreJs = openlawCore.js

git.useGitDescribe := true
val root = (project in file("."))
  .settings(Seq(publish / skip := true))
  .dependsOn(openlawCoreJvm, openlawCoreJs)
  .aggregate(openlawCoreJvm, openlawCoreJs)
  .enablePlugins(GitVersioning)

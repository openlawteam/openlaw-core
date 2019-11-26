import sbtcrossproject.CrossPlugin.autoImport.{CrossType, crossProject}
import ReleaseTransformations._
import sbt.Keys.name

import scala.language.postfixOps
import sbt.{file, _}

lazy val username = "openlaw"
lazy val repo     = "openlaw-core"

licenses += ("Apache-2.0", url("https://opensource.org/licenses/Apache-2.0"))

/*
The Scala and SBT versions must be matched to the version of scala-builder used
as the base image of the container. We try to standardize across projects and
always upgrade in a controlled fashion.

If you wish to update either Scala or SBT, please open an issue and and tag
@openlawteam/infra.
*/
lazy val scalaV = "2.12.10"

lazy val scalaJavaTimeV = "2.0.0-RC3"
lazy val catsV = "2.0.0"
lazy val parboiledV = "2.1.8"
lazy val circeV = "0.12.3"
lazy val playJsonV = "2.8.0"
lazy val scalaTagsV = "0.7.0"
lazy val sLoggingV = "0.6.1"
lazy val enumeratumV = "1.5.13"
lazy val scalaCheckV = "1.14.2"
lazy val scalaTestV = "3.2.0-SNAP10"

lazy val repositories = Seq(
  Resolver.jcenterRepo,
  "central" at "http://central.maven.org/maven2/",
  "scalaz-bintray" at "https://dl.bintray.com/scalaz/releases",
  "maven central" at "https://mvnrepository.com/repos/central",
  Resolver.mavenLocal
)

lazy val commonSettings = Seq(
  organization := "org.openlaw",
  name := "openlaw-core",
  scalaVersion := scalaV,
  wartremoverErrors ++= rules,
  scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature", "-language:implicitConversions"),
  javacOptions ++= Seq("-source", "1.8", "-target", "1.8"),
)

lazy val publishSettings = Seq(
  publishArtifact in (Test, packageBin) := true,
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
  //inquireVersions,                        // : ReleaseStep
  //setReleaseVersion,                      // : ReleaseStep
  //commitReleaseVersion,                   // : ReleaseStep, performs the initial git checks
  //tagRelease,                             // : ReleaseStep
  //releaseStepCommandAndRemaining("publish"),
  publishArtifacts,                       // : ReleaseStep,
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
	Wart.OptionPartial)

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
      "io.circe"                %% "circe-core"          % circeV,
      "io.circe"                %% "circe-generic"       % circeV,
      "io.circe"                %% "circe-parser"        % circeV,
      //parser / interpreter library. Used for our markup language
      "org.parboiled"           %% "parboiled"           % parboiledV,
      //cats is used for FP constructs
      "org.typelevel"           %% "cats-core"           % catsV,
      "org.typelevel"           %% "cats-free"           % catsV,
      // scala.js compatible library to use time types
      "io.github.cquiroz"       %% "scala-java-time"     % scalaJavaTimeV,
      // logging library that is compatible with scala.js
      "biz.enef"                %% "slogging-slf4j"      % sLoggingV,
      // enumeratum provides type-safe enumerations with improvements over stdlib enumerations
      "com.beachape"            %% "enumeratum"          % enumeratumV,
      // scalatags is used for composition of XHTML documents in the document printers
      "com.lihaoyi"             %% "scalatags"           % scalaTagsV,
      //Test
      "org.scalacheck"          %% "scalacheck"          % scalaCheckV % Test,
      "org.scalatest"           %% "scalatest"           % scalaTestV  % Test,
      //Play json is used in tests to make it easier to prepare json in the tests. It shouldn't be used in the library
      "com.typesafe.play"       %% "play-json"           % playJsonV % Test
    )
  ).jsSettings(
    libraryDependencies ++= Seq(
      //circe is used to serialize / deserialize json
      "io.circe"                %%% "circe-core"           % circeV,
      "io.circe"                %%% "circe-generic"        % circeV,
      "io.circe"                %%% "circe-parser"         % circeV,
      //parser / interpreter library. Used for our markup language
      "org.parboiled"           %%% "parboiled"            % parboiledV,
      //cats is used for FP constructs
      "org.typelevel"           %%% "cats-core"            % catsV,
      "org.typelevel"           %%% "cats-free"            % catsV,
      // scala.js compatible library to use time types
      "io.github.cquiroz"       %%% "scala-java-time"      % scalaJavaTimeV,
      // timezone handling is in a separate library because of its size and not everybody needs it. we do
      "io.github.cquiroz"       %%% "scala-java-time-tzdb" % "2.0.0-RC3_2019a",
      // logging library that is compatible with scala.js
      "biz.enef"                %%% "slogging"             % sLoggingV,
      // enumeratum provides type-safe enumerations with improvements over stdlib enumerations
      "com.beachape"            %%% "enumeratum"           % enumeratumV,
      // scalatags is used for composition of XHTML documents in the document printers
      "com.lihaoyi"             %%% "scalatags"            % scalaTagsV,
      //Test
      "org.scalacheck"          %%% "scalacheck"          % scalaCheckV % Test,
      "org.scalatest"           %%% "scalatest"           % scalaTestV  % Test,
      //Play json is used in tests to make it easier to prepare json in the tests. It shouldn't be used in the library
      "com.typesafe.play"       %%% "play-json"           % playJsonV % Test
    )
  )
  .settings(parallelExecution in Test := false)
  .settings(commonSettings: _*)
  .settings(publishSettings: _*)
  .settings(releaseSettings: _*)
  .enablePlugins(WartRemover)

lazy val version = git.gitDescribedVersion

// need commands for releasing both Scala & ScalaJS to avoid issue where release-with-defaults only releases Scala lib

// the next-version flag is to silence SBT's interactive release shell prompt - it doesn't actually alter the version
// confirmed via running `sbt version` after release.
addCommandAlias("releaseCore", ";project openlawCore ;release release-version ${version} next-version ${version-SNAPSHOT} with-defaults")
addCommandAlias("releaseCoreJS", ";project openlawCoreJS ;release release-version ${version} next-version ${version-SNAPSHOT} with-defaults")
addCommandAlias("releaseBoth", ";releaseCore ;releaseCoreJS")

lazy val openlawCoreJvm = openlawCore.jvm
lazy val openlawCoreJs = openlawCore.js

git.useGitDescribe := true

val root = (project in file("."))
  .dependsOn(openlawCoreJvm, openlawCoreJs)
  .aggregate(openlawCoreJvm, openlawCoreJs)
  .enablePlugins(GitVersioning)

logLevel := Level.Warn

resolvers += "Typesafe Repository" at "https://repo.typesafe.com/typesafe/releases/"
resolvers += Resolver.sonatypeRepo("public")

addSbtPlugin("org.lyranthe.sbt" % "partial-unification" % "1.1.0")
addSbtPlugin("org.wartremover" % "sbt-wartremover" % "2.4.2")

/* ScalaJS related */
addSbtPlugin("org.scala-js" % "sbt-scalajs" % "0.6.32")
addSbtPlugin("org.portable-scala" % "sbt-scalajs-crossproject" % "0.6.0")

/* Release process */
addSbtPlugin("com.typesafe.sbt" % "sbt-git" % "1.0.0")
addSbtPlugin("com.github.gseitz" % "sbt-release" % "1.0.7")
addSbtPlugin("com.timushev.sbt" % "sbt-updates" % "0.3.4")
addSbtPlugin("au.com.onegeek" % "sbt-dotenv" % "2.0.117")
addSbtPlugin("com.codecommit" % "sbt-github-packages" % "0.5.3")

/* Code coverage */
addSbtPlugin("org.scoverage" % "sbt-scoverage" % "1.5.1")
addSbtPlugin("com.codacy" % "sbt-codacy-coverage" % "1.3.15")

/* Github Packages */
addSbtPlugin("com.codecommit" % "sbt-github-packages" % "0.5.3")

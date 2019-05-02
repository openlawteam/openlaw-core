logLevel := Level.Warn

resolvers += "Typesafe Repository" at "https://repo.typesafe.com/typesafe/releases/"
resolvers += "Bintray sbt-reactjs" at "https://dl.bintray.com/ddispaltro/sbt-plugins/"
resolvers += Resolver.url("sbt-plugins", url("https://dl.bintray.com/ssidorenko/sbt-plugins/"))(Resolver.ivyStylePatterns)

addSbtPlugin("org.lyranthe.sbt" % "partial-unification" % "1.1.0")
addSbtPlugin("org.wartremover" % "sbt-wartremover" % "2.4.1")

addSbtPlugin("com.timushev.sbt" % "sbt-updates" % "0.3.4")

/* ScalaJS related */
addSbtPlugin("org.scala-js" % "sbt-scalajs" % "0.6.26")
addSbtPlugin("org.portable-scala" % "sbt-scalajs-crossproject" % "0.6.0")

/* Release process */
addSbtPlugin("com.typesafe.sbt" % "sbt-git" % "1.0.0")
addSbtPlugin("org.foundweekends" % "sbt-bintray" % "0.5.4")
addSbtPlugin("com.github.gseitz" % "sbt-release" % "1.0.11")

/* Code coverage */
addSbtPlugin("org.scoverage" % "sbt-scoverage" % "1.5.1")
addSbtPlugin("com.codacy" % "sbt-codacy-coverage" % "1.3.15")

/* TODO: THESE ARE GOING TO BE REMOVED SINCE THEY ARE NOT USED */
addSbtPlugin("org.scalastyle" %% "scalastyle-sbt-plugin" % "1.0.0")

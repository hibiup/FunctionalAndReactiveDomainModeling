val scalaTestVersion = "3.0.5"
val shapelessVersion = "2.3.3"
val scalaCheckVersion = "1.14.0"
val scalazVersion = "7.2.27"
val catsVersion = "1.6.0"
val catsEffectVersion = "1.2.0"
val logBackVersion = "1.2.3"
val scalaLogging = "3.9.2"

lazy val ShapelessGuide = (project in file(".")).
        settings(
            organization := "com.hibiup",
            name := "FunctionalAndReactiveDomainModeling",
            version := "0.1",
            scalaVersion := "2.12.8",
            libraryDependencies ++= Seq(
                "org.scalatest" %% "scalatest" % scalaTestVersion % Test,
                "org.scalacheck" %% "scalacheck" % scalaCheckVersion % Test,
                "org.scalaz" %% "scalaz-core" % scalazVersion,
                "org.scalaz" %% "scalaz-concurrent" % scalazVersion,
                "org.typelevel" %% "cats-core" % catsVersion,
                "org.typelevel" %% "cats-effect" % catsEffectVersion,
                "ch.qos.logback" % "logback-classic" % logBackVersion,
                "com.typesafe.scala-logging" %% "scala-logging" % scalaLogging
            ),
            addCompilerPlugin("org.scalamacros" %% "paradise" % "2.1.1" cross CrossVersion.full),
            addCompilerPlugin("org.scalameta" %% "paradise" % "3.0.0-M11" cross CrossVersion.full),
            scalacOptions ++= Seq(
                "-Xplugin-require:macroparadise",
                "-language:higherKinds",
                "-deprecation",
                "-encoding", "UTF-8",
                "-Ypartial-unification",
                "-feature",
                "-language:_"
            ),
            javacOptions ++= Seq(
                "-Xlint:unchecked"
            )
        )
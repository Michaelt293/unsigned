import sbt._

object Dependencies {
  lazy val scalaTest   = "org.scalatest"  %% "scalatest"    % "3.0.5"
  lazy val catsCore    = "org.typelevel"  %% "cats-core"    % "1.2.0"
  lazy val catsMacros  = "org.typelevel"  %% "cats-macros"  % "1.2.0"
  lazy val catsKernel  = "org.typelevel"  %% "cats-kernel"  % "1.2.0"
  lazy val catsTestkit = "org.typelevel"  %% "cats-testkit" % "1.1.0"
  lazy val scalacheck  = "org.scalacheck" %% "scalacheck"   % "1.14.0"
}

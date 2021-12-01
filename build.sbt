name := "advent-of-code-2021"

version := "0.1"

scalaVersion := "3.1.0"

lazy val versions = new {
  val zio = "2.0.0-M6-2"
}

libraryDependencies ++= Seq(
  "dev.zio"                       %% "zio"                           % versions.zio,
  "dev.zio"                       %% "zio-test"                      % versions.zio,
  "dev.zio"                       %% "zio-streams"                   % versions.zio,
  "dev.zio"                       %% "zio-test-junit"                % versions.zio % Test,
  "dev.zio"                       %% "zio-test-sbt"                  % versions.zio % Test,
)


testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework")

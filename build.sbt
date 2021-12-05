lazy val root = (project in file("."))
  .settings(
    name := "advent-of-code-2021",
//    scalaVersion := "2.13.7",
    scalaVersion := "3.0.2",
    Compile / scalaSource := baseDirectory.value / "src" / "scala",
    Compile / resourceDirectory := baseDirectory.value / "resources"
  )

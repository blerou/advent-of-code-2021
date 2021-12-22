lazy val root = (project in file("."))
  .settings(
    name := "advent-of-code-2021",
    scalaVersion := "3.1.0",
    Compile / scalaSource := baseDirectory.value / "src" / "scala",
    Compile / resourceDirectory := baseDirectory.value / "resources"
  )

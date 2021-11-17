name := "SCL"

version := "0.1"

scalaVersion := "2.13.6"

assembly / mainClass := Some("Launcher")

assembly / test := (Test / test).value

    // more settings here ...



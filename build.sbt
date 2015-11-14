name := "idl2js"

version := "0.1.0"

scalaVersion := "2.11.7"

libraryDependencies += "org.apache.axis2" % "axis2-corba" % "1.6.3"

fork in run := true

lazy val root = (project in file("."))
    .enablePlugins(BuildInfoPlugin)
    .settings(
        buildInfoKeys := Seq[BuildInfoKey](name,
                                           version,
                                           scalaVersion,
                                           sbtVersion),
        buildInfoPackage := "pl.edu.uj.synchrotron.idl2js"
    )

EclipseKeys.createSrc := EclipseCreateSrc.Default + EclipseCreateSrc.Managed

packAutoSettings

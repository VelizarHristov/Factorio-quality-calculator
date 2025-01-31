import org.scalajs.linker.interface.ModuleSplitStyle

val scala3Version = "3.6.3"

lazy val root = project
  .in(file("."))
  .enablePlugins(ScalaJSPlugin)
  .enablePlugins(BuildInfoPlugin)
  .settings(
    name := "FactorioQualityCalculator",
    version := "0.2",
    scalaVersion := scala3Version,
    buildInfoKeys := Seq[BuildInfoKey](version),
    buildInfoPackage := "calculator",

    scalaJSUseMainModuleInitializer := true,

    scalaJSLinkerConfig ~= {
      _.withModuleKind(ModuleKind.ESModule)
        .withModuleSplitStyle(
          ModuleSplitStyle.SmallModulesFor(List("calculator")))
    },

    libraryDependencies += "org.scalameta" %% "munit" % "1.0.0" % Test,
    libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "2.8.0",
    libraryDependencies += "com.raquo" %%% "laminar" % "17.2.0",
  )

import org.scalajs.linker.interface.ModuleSplitStyle

val scala3Version = "3.6.3"
val compilerOptions = Seq(
  "-unchecked",
  "-deprecation",
  "-Wunused:all",
  "-Xlint:all"
)

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
    scalacOptions ++= compilerOptions,

    scalaJSUseMainModuleInitializer := true,

    scalaJSLinkerConfig ~= {
      _.withModuleKind(ModuleKind.ESModule)
        .withModuleSplitStyle(
          ModuleSplitStyle.SmallModulesFor(List("calculator")))
    },

    libraryDependencies += "org.scalameta" %% "munit" % "1.0.0" % Test,
    libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "2.8.0",
    libraryDependencies += "com.raquo" %%% "laminar" % "17.2.0",
    libraryDependencies += "com.lihaoyi" %%% "upickle" % "4.1.0",
    libraryDependencies += "be.doeraene" %%% "web-components-ui5" % "2.1.0"
  )

lazy val preprocessor = project
  .in(file("preprocessing"))
  .settings(
    name := "DataPreprocessor",
    version := "1.0",
    scalaVersion := scala3Version,
    scalacOptions ++= compilerOptions,

    libraryDependencies += "com.lihaoyi" %% "upickle" % "4.1.0"
  )

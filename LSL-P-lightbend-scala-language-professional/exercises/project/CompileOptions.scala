
object CompileOptions {
  val compileOptions = Seq(
    "-unchecked",
    "-deprecation",
    "-feature",
    "-language:postfixOps",
    "-language:implicitConversions",
    "-language:reflectiveCalls",
    "-target:jvm-1.8",
    "-encoding", "UTF-8"
  )
}

package adventofcode
package utilities

object AdventOfCode:
  export Env.Prod
  export Env.Test

  enum Env(val file: String):
    case Test extends Env("test")
    case Prod extends Env("input")

  trait AdventOfCode(private val env: Env) extends App:
    val day: String =
      this
        .getClass
        .getName
        .init
        .toLowerCase
        .replace('.', '/')

    import scala.io.*
    import scala.util.Using

    lazy val getEnv: Env =
      sys.env.get("ENV") match
        case None      => env
        case Some(env) => Env.valueOf(env)

    lazy val input: String =
      Using.resource(Source.fromResource(s"$day.${getEnv.file}")):
        (i: BufferedSource) => i.mkString

    def answer[A](part: Int)(a: => A): Unit =
      val startTime: Long =
        System.currentTimeMillis

      val envs: List[String] = List(
        s"${Console.YELLOW} ${Env.fromOrdinal(getEnv.ordinal)} ${Console.RESET}",
        s"${Console.GREEN} ${Env.fromOrdinal(getEnv.ordinal)} ${Console.RESET}"
      )

      println:
        s"${envs(getEnv.ordinal)}The answer to $day part $part is: " +
          s"${Console.BLUE}${a.toString}${Console.RESET} [${System.currentTimeMillis - startTime}ms]"

package adventofcode
package utilities

import java.io.File
import java.io.FileWriter
import java.net.HttpURLConnection
import java.net.URI
import scala.io.Source
import scala.util.Using

object AdventOfCode:
  export Env.Prod
  export Env.Test
  export Utilities.dump

  enum Env(val file: String):
    case Test extends Env("test")
    case Prod extends Env("input")

  trait AdventOfCode(private val env: Env) extends App:
    lazy val day: Int =
      getClass
        .getName
        .init
        .split('.')
        .last
        .drop(3)
        .toInt

    lazy val year: Int =
      getClass
        .getName
        .init
        .split('.')
        .dropRight(1)
        .last
        .drop(3)
        .toInt

    lazy val getEnv: Env =
      sys
        .env
        .get("ENV")
        .filter(_.nonEmpty)
        .fold(env)(Env.valueOf)

    lazy val input: String =
      val root =
        File("")
          .getAbsolutePath
          .split('/')
          .takeWhile(_ != "code")
          .mkString("/")

      Using.resource(
        Source.fromFile:
          createIfNotExists:
            new File(
              s"$root/code/$year/src/main/" +
                f"resources/day$day%02d.${getEnv.file}"
            )
      )(_.mkString)

    def answer[A](part: Int)(a: => A): Unit =
      val startTime: Long =
        System.currentTimeMillis

      val envs: List[String] = List(
        s"${Console.YELLOW} ${Env.fromOrdinal(getEnv.ordinal)} ${Console.RESET}",
        s"${Console.GREEN} ${Env.fromOrdinal(getEnv.ordinal)} ${Console.RESET}"
      )

      println:
        s"${envs(getEnv.ordinal)} The answer to $year day $day part $part is: " +
          s"${Console.BLUE}${a.toString}${Console.RESET} [${System.currentTimeMillis -
              startTime}ms]"

    private def createIfNotExists(file: File): File =
      if (!file.createNewFile && file.length() != 0) || getEnv == Env.Test then
        file
      else
        URI(
          s"https://adventofcode.com/$year/day/$day/input"
        ).toURL.openConnection match
          case conn: HttpURLConnection =>
            println:
              s"${Console.RED} Downloading input file for $year day $day ${Console.RESET}"

            conn.setRequestProperty("Cookie", s"session=${sys.env("SESSION")}")
            conn.setRequestProperty(
              "User-Agent",
              s"https://github.com/thijsnissen/advent-of-code by ${sys.env("EMAIL")}"
            )

            Using.resource(
              Source.fromInputStream(conn.getInputStream, "UTF-8")
            ): source =>
              Using.resource(new FileWriter(file))(_.write(source.mkString))

        file

trait AdventOfCode extends App:
	enum Mode(val file: String):
		case Test extends Mode("test")
		case Prod extends Mode("input")

	val day: String =
		this.getClass.getName.init.toLowerCase

	import scala.io.Source

	def input(using mode: Mode): Iterator[String] =
		Source
			.fromResource(s"$day-${mode.file}.txt")
			.getLines

	def answer[A](part: Int)(a: => A)(using mode: Mode): Unit =
		val startTime: Long =
			System.currentTimeMillis

		val modes: List[String] = List(
			s"${Console.YELLOW} ${Mode.fromOrdinal(mode.ordinal)} ${Console.RESET}",
			s"${Console.GREEN} ${Mode.fromOrdinal(mode.ordinal)} ${Console.RESET}"
		)

		println(s"${modes(mode.ordinal)}The answer to $day part $part is: " +
			s"${Console.BLUE}$a${Console.RESET} [${System.currentTimeMillis - startTime}ms]")

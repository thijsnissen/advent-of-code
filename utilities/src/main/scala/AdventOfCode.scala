trait AdventOfCode extends App:
	enum Mode(val m: String):
		case Test extends Mode("test")
		case Prod extends Mode("input")

	val day: String =
		this.getClass.getName.init.toLowerCase

	import scala.io.Source

	def input(using mode: Mode): Iterator[String] =
		Source
			.fromResource(s"$day-${mode.m}.txt")
			.getLines

	def answer[A](part: Int)(a: => A): Unit =
		val startTime: Long =
			System.currentTimeMillis

		println(s"The answer to $day part $part is: $a [${System.currentTimeMillis - startTime}ms]")

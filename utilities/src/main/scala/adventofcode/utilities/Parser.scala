package adventofcode
package utilities

opaque type Parser[+A] =
	String => Either[Parser.Error, (A, String)]

object Parser:
	opaque type Error = String

	def succeed[A](a: A): Parser[A] =
		(s: String) => Right(a, s)

	def fail[A](msg: String): Parser[A] =
		(_: String) => Left(msg)

	def getNext[A]: Parser[Char] =
		(s: String) =>
			if s.nonEmpty then
				Right(s.head, s.tail)
			else
				Left("Could not parse empty string")

	def satisfy(f: Char => Boolean): Parser[Char] =
		getNext.flatMap: c =>
			if f(c) then
				succeed(c)
			else
				fail(s"Could not parse character: $c")

	def char(c: Char): Parser[Char] =
		satisfy(_ == c)

	def string(s: String): Parser[String] =
		if s.isEmpty then
			succeed("")
		else
			for
				_ <- char(s.head)
				_ <- string(s.tail)
			yield
				s

	def oneOf(s: String): Parser[Char] =
		satisfy(s.contains)

	import scala.util.matching.Regex

	def regex(r: Regex): Parser[String] =
		(s: String) => r.findPrefixOf(s) match
			case Some(m) => succeed(m)(s.substring(m.length))
			case None => fail(s"Could not match regex '${r.toString}' with string: ${s.take(10)}...")("")

	def whitespace: Parser[String] =
		regex("[\\u0020\\u000A\\u000D\\u0009]*".r)

	extension[A] (self: Parser[A])
		def run(s: String): Either[Error, A] =
			self(s) match
				case Right(a, "") => Right(a)
				case Right(a, r) => Left(s"Unconsumed characters found: $r")
				case Left(e) => Left(e)

		def flatMap[B](f: A => Parser[B]): Parser[B] =
			(s: String) => self(s) match
				case Right(a, r) => f(a)(r)
				case Left(e) => Left(e)

		def map[B](f: A => B): Parser[B] =
			(s: String) => self(s) match
				case Right(a, r) => succeed(f(a))(r)
				case Left(e) => Left(e)

		def mapTwo[B, C](that: => Parser[B])(f: (A, B) => C): Parser[C] =
			for
				a <- self
				b <- that
			yield
				f(a, b)

		def zeroOrMore: Parser[List[A]] =
			self.oneOrMore | succeed(Nil)

		def oneOrMore: Parser[List[A]] =
			self.mapTwo(self.zeroOrMore)(_ :: _)

		def times(n: Int): Parser[List[A]] =
			if n <= 0 then
				succeed(Nil)
			else
				self.mapTwo(self.times(n - 1))(_ :: _)

		def separator[B](sep: Parser[B]): Parser[List[A]] =
			self.mapTwo((sep ~ self).zeroOrMore)(_ :: _)

		def token: Parser[A] =
			for
				a <- self
				_ <- whitespace
			yield
				a

		@annotation.targetName("or")
		def |[B >: A](that: => Parser[B]): Parser[B] =
			(s: String) => self(s) match
				case Left(_) => that(s)
				case Right(a, r) => succeed(a)(r)

		@annotation.targetName("product")
		def &[B](that: => Parser[B]): Parser[(A, B)] =
			self.mapTwo(that)((_, _))

		@annotation.targetName("ignoreLeft")
		def ~[B](that: => Parser[B]): Parser[B] =
			self.mapTwo(that)((_, b) => b)

	extension[A] (self: Parser[List[A]])
		def asString: Parser[String] =
			self.map(l => l.mkString)

	given fromStringToParser: Conversion[String, Parser[String]] with
		def apply(s: String): Parser[String] = string(s)

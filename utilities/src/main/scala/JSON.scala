enum JSON:
	case JNull
	case JNumber(get: Double)
	case JString(get: String)
	case JBool(get: Boolean)
	case JArray(get: IndexedSeq[JSON])
	case JObject(get: Map[String, JSON])

object JSON:
	import Parser.*

	val jnull: Parser[JSON] =
		string("null").map(_ => JNull)

	val jbool: Parser[JBool] =
		(string("true") | string("false")).map(b => JBool(b.toBoolean))

	val jnumber: Parser[JNumber] =
		regex("[-+]?([0-9]*\\.)?[0-9]+([eE][-+]?[0-9]+)?".r).map(n => JNumber(n.toDouble))

	val jstring: Parser[JString] =
		for
			_ <- whitespace ~ char('"')
			s <- regex("[^\"]*".r)
			_ <- char('"')
		yield
			JString(s)

	val jarray: Parser[JArray] =
		for
			_ <- whitespace ~ char('[').token
			a <- json.separator(char(','))
			_ <- whitespace ~ char(']').token
		yield
			JArray(a.toIndexedSeq)

	val keyval: Parser[(String, JSON)] =
		for
			k <- whitespace ~ jstring
			_ <- whitespace ~ char(':')
			v <- whitespace ~ json
		yield
			(k.get, v)

	val jobject: Parser[JObject] =
		for
			_ <- whitespace ~ char('{').token
			o <- keyval.separator(char(','))
			_ <- whitespace ~ char('}').token
		yield
			JObject(o.toMap)

	def json: Parser[JSON] =
		jnull | jnumber | jstring | jbool | jarray | jobject

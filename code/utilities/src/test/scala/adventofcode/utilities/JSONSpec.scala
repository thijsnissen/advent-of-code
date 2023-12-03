package adventofcode
package utilities

import org.scalatest.funsuite.AnyFunSuite

class JSONSpec extends AnyFunSuite:
  test("JSON"):
    val jsonTxt =
      """
				|{
				|	"Company name" : "Microsoft Corporation",
				|	"Ticker"  : "MSFT",
				|	"Active"  : true,
				|
				|	"Price"   : 30.66,
				|	"Shares outstanding" : 8.38e9,
				|	"Related companies" :
				|
				|        [ "HPQ", "IBM", "YHOO", "DELL", "GOOG" ]
				|
				|}
				|
				|""".stripMargin

    val malformedJson =
      """
				|{
				|	"Company name" ; "Microsoft Corporation"
				|}
				|
				|""".stripMargin

    import JSON.*

    val succJson: JSON = JSON.json.run(jsonTxt) match
      case Right(json) => json
      case Left(e)     => JString(e.toString)

    assertResult(JSON.json.run(jsonTxt))(JSON.json.run(succJson.asString))
    assertResult(
      Parser.fail("Could not parse character: ;").run(malformedJson)
    )(JSON.json.run(malformedJson))

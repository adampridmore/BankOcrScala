import scala.io.Source

class BankOrcSpec extends org.scalatest.FunSuite {
  def loadFile(filename: String) = {
    val source = Source.fromFile(filename)
    val text = source.getLines.mkString("\n")
    source.close()
    text
  }

  def bankOcr(text: String): String = {
    val numbers = Map(
      "     |  |" -> "1",
      " _  _||_ " -> "2",
      " _  _| _|" -> "3",
      "   |_|  |" -> "4",
      " _ |_  _|" -> "5",
      " _ |_ |_|" -> "6",
      " _   |  |" -> "7",
      " _ |_||_|" -> "8",
      " _ |_| _|" -> "9"
    )

    val blockedLines =
      text
        .split(System.lineSeparator())
        .map((line: String) => line.grouped(3).map(group => group.mkString))

    blockedLines(0)
      .zip(blockedLines(1))
      .zip(blockedLines(2))
      .map({ case ((blockLine1: String, blockLine2: String), blockLine3: String) => blockLine1 + blockLine2 + blockLine3 })
      .map(number => numbers(number))
      .mkString
  }

  test("Parse a 1") {
    val text = loadFile("one")

    assert(bankOcr(text) === "1")
  }

  test("Parse a 2") {
    val text = loadFile("two")

    assert(bankOcr(text) === "2")
  }

  test("Parse a lot of 1's") {
    val text = loadFile("manyones")

    assert(bankOcr(text) === "1111")
  }
  test("Parse 123456789") {
    val text = loadFile("123456789")

    assert(bankOcr(text) === "123456789")
  }


  //  test("Parse Test") {
  //    val text = loadFile("text")
  //
  //    assert(bankOcr(text) === "1")
  //  }
}

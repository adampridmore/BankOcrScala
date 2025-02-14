import BankOcr.bankOcrParse
import org.scalatest.funsuite.AnyFunSuite

import scala.io.Source

class BankOrcSpec extends AnyFunSuite {
  private def loadFile(filename: String) = {
    val source = Source.fromFile(filename)
    val text = source.getLines.mkString("\n")
    source.close()
    text
  }

  test("Parse a 1") {
    val text = loadFile("one")

    assert(bankOcrParse(text) === "1")
  }

  test("Parse a 2") {
    val text = loadFile("two")

    assert(bankOcrParse(text) === "2")
  }

  test("Parse a lot of 1's") {
    val text = loadFile("manyones")

    assert(bankOcrParse(text) === "1111")
  }
  test("Parse 123456789") {
    val text = loadFile("123456789")

    assert(bankOcrParse(text) === "123456789")
  }
}

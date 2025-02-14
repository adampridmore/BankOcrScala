import BankOcr.bankOcrParse
import BankOcr.isValidNumber
import org.scalatest.wordspec.AnyWordSpec

import scala.io.Source

class BankOrcSpec extends AnyWordSpec {
  private def loadFile(filename: String) = {
    val source = Source.fromFile(filename)
    val text = source.getLines.mkString("\n")
    source.close()
    text
  }

  "Use case 1" can {
    "Parse a 1" in {
      val text = loadFile("one")

      assert(bankOcrParse(text) === "1")
    }

    "Parse a 2" in {
      val text = loadFile("two")

      assert(bankOcrParse(text) === "2")
    }

    "Parse a lot of 1's" in {
      val text = loadFile("manyones")

      assert(bankOcrParse(text) === "1111")
    }

    "Parse 123456789" in {
      val text = loadFile("123456789")

      assert(bankOcrParse(text) === "123456789")
    }
  }

  "Use case 2" can {
    "for a valid number" in {
      assert(isValidNumber("345882865") === true)
    }
    "for an invalid number" in {
      assert(isValidNumber("111111111") === false)
    }
  }
}
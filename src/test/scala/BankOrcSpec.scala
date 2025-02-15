import AccountNumber.bankOcrParse
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

      assert(bankOcrParse(text) === AccountNumber("1"))
    }

    "Parse a 2" in {
      val text = loadFile("two")

      assert(bankOcrParse(text) === AccountNumber("2"))
    }

    "Parse a lot of 1's" in {
      val text = loadFile("manyones")

      assert(bankOcrParse(text) === AccountNumber("1111"))
    }

    "Parse 123456789" in {
      val text = loadFile("123456789")

      assert(bankOcrParse(text) === AccountNumber("123456789"))
    }

    "Parse a 0" in {
      val text = loadFile("zero")

      assert(bankOcrParse(text) === AccountNumber("0"))
    }
  }

  "Use case 2" can {
    "for a valid number" in {
      assert(AccountNumber("345882865").isValidNumber === true)
    }
    "for an invalid number" in {
      assert(AccountNumber("111111111").isValidNumber === false)
    }
  }

  "User case 3" can {
    "valid number" in {
      assert(AccountNumber("345882865").report === ReportLine("345882865"))
    }

    "error number" in {
      assert(AccountNumber("664371495").report === ReportLine("664371495 ERR"))
    }

    "Illegal number" in {
      assert(AccountNumber("86110??36").report === ReportLine("86110??36 ILL"))
    }
  }
}
import BankOcr.{AccountNumber, ReportLine, bankOcrParse, isValidNumber, report}
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
  }

  "Use case 2" can {
    "for a valid number" in {
      assert(isValidNumber(AccountNumber("345882865")) === true)
    }
    "for an invalid number" in {
      assert(isValidNumber(AccountNumber("111111111")) === false)
    }
  }

  "User case 3" can {
    "valid number" in {
      assert(report(AccountNumber("345882865")) === ReportLine("345882865"))
    }

    "error number" in {
      assert(report(AccountNumber("664371495")) === ReportLine("664371495 ERR"))
    }

    "Illegal number" in {
      assert(report(AccountNumber("86110??36")) === ReportLine("86110??36 ILL"))
    }
  }
}
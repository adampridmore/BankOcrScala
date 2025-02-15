object BankOcr {

  case class AccountNumber(digits: String)

  class ReportLine(val value: String) extends AnyVal

  def bankOcrParse(textToOcr: String): AccountNumber = {
    val blockedLines =
      textToOcr
        .split(System.lineSeparator())
        .map((line: String) => line.grouped(3).map(group => group.mkString).toSeq)

    AccountNumber((blockedLines(0), blockedLines(1), blockedLines(2))
      .zipped
      .map({case a => a.productIterator.mkString})
      .map(numberBlock => numbers(numberBlock))
      .mkString)
  }

  def isValidNumber(accountNumber: AccountNumber): Boolean = {
    //    (d1+2*d2+3*d3 +..+9*d9) mod 11 = 0

    val total = accountNumber.digits.toCharArray
          .reverse
          .zipWithIndex
          .map((digit, index) => ( digit.toString.toInt .toInt, index))
          .map((digit,index) => digit * (index+1))
          .sum

    (total % 11) == 0
  }

  def report(accountNumber: AccountNumber) : ReportLine = {
    if (accountNumber.digits.contains("?")){
      ReportLine(accountNumber.digits + " ILL")
    } else if (!isValidNumber(accountNumber)){
      ReportLine(accountNumber.digits + " ERR")
    } else {
      ReportLine(accountNumber.digits)
    }
  }

  private val numbers = Map(
    "   " +
    "  |" +
    "  |" -> "1",

    " _ " +
    " _|" +
    "|_ " -> "2",

    " _ " +
    " _|" +
    " _|" -> "3",

    "   " +
    "|_|" +
    "  |" -> "4",

    " _ " +
    "|_ " +
    " _|" -> "5",

    " _ " +
    "|_ " +
    "|_|" -> "6",

    " _ " +
    "  |" +
    "  |" -> "7",

    " _ " +
    "|_|" +
    "|_|" -> "8",

    " _ " +
    "|_|" +
    " _|" -> "9"
  )
}
class ReportLine(val value: String) extends AnyVal

case class AccountNumber(digits: String) {
  def isValidNumber: Boolean = {
    //    (d1+2*d2+3*d3 +..+9*d9) mod 11 = 0

    val total = digits.toCharArray.reverse
      .zipWithIndex
      .map((digit, index) => (digit.toString.toInt, index))
      .map((digit, index) => digit * (index + 1))
      .sum

    (total % 11) == 0
  }

  def report: ReportLine = {
    if (digits.contains("?")) {
      ReportLine(digits + " ILL")
    } else if (!isValidNumber) {
      ReportLine(digits + " ERR")
    } else {
      ReportLine(digits)
    }
  }
}

object AccountNumber {

  def bankOcrParse(textToOcr: String): AccountNumber = {
    val blockedLines =
      textToOcr
        .split(System.lineSeparator())
        .map((line: String) => line.grouped(3).map(group => group.mkString).toSeq)

    AccountNumber(
      blockedLines(0)
        .lazyZip(blockedLines(1))
        .lazyZip(blockedLines(2))
        .map((a, b, c) => s"$a$b$c")
        .map(numbers)
        .mkString
    )
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
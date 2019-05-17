object BankOcr {
  def bankOcrParse(text: String): String = {
    val blockedLines =
      text
        .split(System.lineSeparator())
        .map((line: String) => line.grouped(3).map(group => group.mkString).toSeq)

    (blockedLines(0), blockedLines(1), blockedLines(2))
      .zipped
      .map({case a => a.productIterator.mkString})
      .map(numberBlock => numbers(numberBlock))
      .mkString
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
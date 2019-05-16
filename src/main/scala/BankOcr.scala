object BankOcr {
  def bankOcrParse(text: String): String = {
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
      .map(numberBlock => numbers(numberBlock))
      .mkString
  }
}
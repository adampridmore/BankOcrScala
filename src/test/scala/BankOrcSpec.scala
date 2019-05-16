import scala.io.Source

class BankOrcSpec extends org.scalatest.FunSuite {
  def loadFile(filename: String) = {
    val source = Source.fromFile(filename)
    val text = source.getLines.mkString("\n")
    source.close()
    text
  }

  val one = "     |  |"
  val two = " _  _||_ "

  def bankOcr(text: String): String = {

    println("Text: \n" + text)

    val blockedLines: Array[Array[String]] =
      text
        .split("\n")
        .map((line: String) => line.grouped(3).map(group => group.mkString).toArray)
        .toArray

    println("0:" + blockedLines(0).mkString(","))
    println("1:" + blockedLines(1).mkString(","))
    println("2:" + blockedLines(2).mkString(","))

    //    println("***" + blockedLines.map(x => x).mkString(""))

    println(blockedLines(0)
      .zip(blockedLines(1))
      .zip(blockedLines(2).map(a => a))
    )

    val strings: Array[String] =
      blockedLines(0)
        .zip(blockedLines(1))
        .zip(blockedLines(2))
        .map({ case ((a: String, b: String), c: String) => a + b + c })
        .toArray

    strings.map(block => {
      block match {
        case x if x == one => "1"
        case x if x == two => "2"
        case x => throw new RuntimeException(s"Unknown block: [$x]")
      }
    }).mkString("")
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


  test("Parse Test") {
    val text = loadFile("text")

    assert(bankOcr(text) === "1")
  }
}

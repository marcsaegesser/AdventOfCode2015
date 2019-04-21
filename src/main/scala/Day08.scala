package advent

object Day08 {

  def day08(): Unit = {
    val input = readFile(inputFile)
    println(s"Day08.part1 = ${part1(input)}")
    println(s"Day08.part2 = ${part2(input)}")
  }

  def part1(input: List[String]): Int =
    input.foldLeft(0) { case (a, s) => a + (s.size - parseString(s).size) }

  def part2(input: List[String]): Int =
    input.foldLeft(0) { case (a, s) => a + (encodeString(s).size - s.size) }

  def encodeString(input: String): String = {
    input.foldLeft(StringBuilder.newBuilder) {
      case (a, '\\') => a append ("""\\""")
      case (a, '"') => a append ("""\"""")
      case (a, c) => a append(c)
    }.mkString("\"", "", "\"")
  }

  def parseString(input: String): String = {
    def helper(accum: List[Char], s: List[Char]): String =
      if(s.isEmpty) accum.drop(1).reverse.mkString
      else {
        val (a, b) = parseChar(s)
        helper(b :: accum, a)
      }

    helper(List.empty[Char], input.toList.drop(1))
  }


  def parseChar(s: List[Char]): (List[Char], Char) = {
    s match {
      case '\\' :: t => parseEsc(t)
      case a :: t    => (t, a)
      case Nil       => throw new Exception("Invalid input")
    }
  }

  def parseEsc(s: List[Char]): (List[Char], Char) =
    s match {
      case '\\' :: t => (t, '\\')
      case '"' :: t  => (t, '"')
      case 'x' :: a :: b :: t if a.isHexDigit && b.isHexDigit => (t, Integer.parseInt(List(a, b).mkString, 16).toChar)
      case _         => throw new Exception("Invalid input")
    }

  implicit class HexParser(val c: Char) extends AnyVal {
    def isHexDigit: Boolean = {
      val lc = c.toLower
      (lc >= '0' && lc <= '9') ||
      (lc >= 'a' && lc <= 'f')
    }
  }

  def readFile(file: String): List[String] =
    io.Source.fromFile(file)
      .getLines()
      .filterNot(_.isEmpty)
      .map(_.trim)
      .toList

  val inputFile = "data/Day08.txt"
}

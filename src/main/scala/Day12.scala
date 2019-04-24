package advent

// Not thrilled with this solution but it worked. Review this one later.
object Day12 {

  val numRegex = """(-?\d+)""".r

  def part1(input: String): Int =
    numRegex.findAllIn(input).map(_.toInt).sum

  def part2(input: String): Int =
    numRegex.findAllIn(parseJson(input)._1).map(_.toInt).sum

  def parseText(input: String): String = ???

  val redRegex = """:\s*"red""".r

  def isRed(s: String): Boolean =
    redRegex.findFirstIn(s).isDefined

  def parseJson(input: String): (String, String) = {
    def helper(accum: String, s: String): (String, String) = {
      val (h, t) = s.span(_ != 0x7B)
      t.headOption match {
        case None => (accum ++ h, t)
        case Some(_) =>
          val (o, r) = parseObject(t.drop(1))
          helper(accum ++ h ++ o, r)
      }
    }

    helper("", input)
  }


  def parseObject(input: String): (String, String) = {
    def helper(accum: String, s: String): (String, String) = {
      val (h, t) = s.span(c => !"{}".contains(c))
      t.head match {
        case 0x7B => // Open brace
          val (o, r) = parseObject(t.drop(1))
          helper(accum ++ h ++ o, r)
        case 0x7D => // Close brace
          val x = accum ++ h
          if(isRed(x)) ("{}", t.drop(1))
          else         (s"{$x}", t.drop(1))
      }
    }

    helper("", input)
  }

  def readFile(file: String): String =
    io.Source.fromFile(file)
      .getLines()
      .toList
      .mkString


  val inputFile = "data/Day12.txt"
}

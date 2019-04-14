package advent

object Day01 {

  def day01(): Unit = {
    val input = readFile(inputFile)
    println(s"Day01.part1 = ${part1(input)}")
    println(s"Day02.part2 = ${part2(input)}")
  }

  def part1(input: String): Int =
    input.foldLeft(0) {
      case (f, 0x28) => f + 1
      case (f, 0x29) => f - 1
      case (f, _)    => f
    }

  def part2(input: String): Int = {
    def helper(i: Int, f: Int, cs: List[Char]): Int =
      if(f < 0) i
      else
        cs match {
          case Nil       => throw new Exception("Never reached the basement")
          case 0x28 :: t => helper(i+1, f+1, t)
          case 0x29 :: t => helper(i+1, f-1, t)
          case _ :: t    => helper(i+1, f, t)
        }

    helper(0, 0, input.toList)
  }

  def readFile(file: String): String =
    io.Source.fromFile(file)
      .getLines
      .toList
      .mkString


  val inputFile = "data/Day01.txt"
}

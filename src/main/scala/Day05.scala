package advent

object Day05 {

  def day05(): Unit = {
    val input = readFile(inputFile)
    println(s"Day05.part1 = ${part1(input)}")
    println(s"Day05.part2 = ${part2(input)}")
  }

  def part1(input: List[String]): Int =
    input
      .filter(isNice)
      .size

  def part2(input: List[String]): Int =
    input
      .filter(isNice2)
      .size

  def isNice(s: String): Boolean ={
    meetsVowelProperty(s)   &&
    meetsDoubleProperty(s)  &&
    meetsExcludedProperty(s)
  }

  def isNice2(s: String): Boolean =
    meetsPairProperty(s) && meetsRepeatAroundProperty(s)

  def meetsVowelProperty(s: String): Boolean =
    s.foldLeft(0) { case (v, c) => if("aeiou".contains(c)) v+1 else v } >= 3

  def meetsDoubleProperty(s: String): Boolean =
    s.zip(s.drop(1)).exists { case (a, b) => a == b }

  val excludedStrings = List("ab", "cd", "pq", "xy")

  def meetsExcludedProperty(s: String): Boolean =
    !excludedStrings.foldLeft(false) { case (e, ex) => e || s.contains(ex) }

  def meetsPairProperty(s: String): Boolean = {
    def helper(l: List[(Char, Char)]): Boolean =
      l match {
        case Nil           => false
        case a :: Nil      => false
        case a :: b :: Nil => false
        case a :: b :: t   => t.contains(a) || helper(b :: t)
      }

    helper(s.zip(s.drop(1)).toList)
  }

  def meetsRepeatAroundProperty(s: String): Boolean =
    s
      .sliding(3)
      .exists(x => x.head == x.last)

  def readFile(file: String): List[String] =
    io.Source.fromFile(file)
      .getLines()
      .toList

  val inputFile = "data/Day05.txt"
}

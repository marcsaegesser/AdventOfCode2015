package advent

object Day02 {

  def part1(input: List[Box]): Int =
    input
      .map(b => b.boxArea + b.minSideArea)
      .sum

  def part2(input: List[Box]): Int =
    input
      .map(b => b.minCircumference + b.volume)
      .sum


  case class Box(l: Int, w: Int, h: Int) {
    val sideArea = l*h
    val topArea =  l*w
    val endArea =  h*w
    val boxArea = 2*sideArea + 2*topArea + 2*endArea
    val minSideArea = List(sideArea, topArea, endArea).min
    val minCircumference = List(2*(h+w), 2*(h+l), 2*(l+w)).min
    val volume = l*w*h
  }

  def readFile(file: String): List[Box] =
    io.Source.fromFile(file)
      .getLines()
      .map(_.split("x").toList)
      .collect { case l :: w :: h :: Nil => Box(l.toInt, w.toInt, h.toInt) }
      .toList

  val inputFile = "data/Day02.txt"
}

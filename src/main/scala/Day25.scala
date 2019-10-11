package advent

object Day25 {

  def day25(): Unit = {
    val (r, c) = readFile(puzzleInput)
    println(s"Day25.part1 = ${part1(r, c)}")
  }

  def part1(r: Int, c: Int): Long =
    codeFor(r, c)

  def codeFor(r: Int, c: Int): Long = {
    codeIter.drop(countRowCol(r, c)-1).next
  }

  def countRowCol(r: Int, c: Int): Int = {
    rowIter(r).drop(c-1).next
  }

  def col1Iter = new Iterator[Int] {
    var v = 1
    var incr = 0

    def hasNext: Boolean = true

    def next(): Int = {
      v = v + incr
      incr = incr + 1
      v
    }
  }

  def rowIter(row: Int) = new Iterator[Int] {
    var v = col1Iter.drop(row-1).next
    var incr = row+1

    def hasNext: Boolean = true

    def next(): Int = {
      val x = v
      v = v + incr
      incr = incr + 1
      x
    }
  }

  def codeIter = new Iterator[Long] {
    var code: Long = 20151125

    def nextCode(code: Long): Long =
      (code * 252533L) % 33554393L

    def hasNext: Boolean = true

    def next(): Long = {
      val c = code
      code = nextCode(code)
      c
    }
  }

  val inputRegex = """To continue, please consult the code grid in the manual.  Enter the code at row (\d+), column (\d+).""".r
  def parseInput(s: String): (Int, Int) = {
    val inputRegex(r, c) = s
    (r.toInt, c.toInt)
  }

  def readFile(f: String): (Int, Int) =
    io.Source.fromFile(f).getLines()
      .map(parseInput)
      .next()

  val puzzleInput = "data/Day25.txt"
}

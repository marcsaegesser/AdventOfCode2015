package advent

object Day18 {

  def day18(): Unit = {
    val grid = readFile(inputFile)
    println(s"Day18.part1 = ${part1(grid)}")
    println(s"Day18.part2 = ${part2(grid)}")
  }

  def part1(grid: Grid): Int =
    runN(100, grid).size

  def part2(grid: Grid): Int =
    runN2(100, grid).size

  def runN(n: Int, grid: Grid): Grid = {
    if(n == 0) grid
    else       runN(n-1, updateGrid(grid))
  }

  def runN2(n: Int, grid: Grid): Grid =
    if(n == 0) turnOnStuckLights(grid)
    else       runN2(n-1, updateGrid(turnOnStuckLights(grid)))

  val allCoords =
    for {
      r <- (0 until 100)
      c <- (0 until 100)
    } yield Coord(r, c)

  val stuckCoords = Set(Coord(0,0), Coord(0,99), Coord(99,0), Coord(99,99))

  def turnOnStuckLights(grid: Grid): Grid =
    grid ++ stuckCoords

  def updateGrid(grid: Grid): Grid = {
    allCoords.foldLeft(Set.empty[Coord]) { case (g, c) =>
      val count = neighborCount(grid, c)
      if(grid(c)) {  // Element is on
        if(count == 2 || count == 3) g + c
        else                        g
      } else {      // Element is off
        if(count == 3) g + c
        else           g
      }
    }
  }

  def neighborCount(grid: Grid, coord: Coord): Int =
    neighbors(coord)
      .filter(c => isOn(grid, c))
      .size

  def neighbors(c: Coord): List[Coord] =
    c match { case Coord(r, c) =>
      List(
        Coord(r-1, c),     // N
        Coord(r-1, c+1),   // NE
        Coord(r, c+1),     // E
        Coord(r+1, c+1),   // SE
        Coord(r+1, c),     // S
        Coord(r+1, c-1),   // SW
        Coord(r, c-1),     // W
        Coord(r-1, c-1)    // NW
      )
    }


  val On = '#'
  val Off = '.'

  case class Coord(r: Int, c: Int)
  type Grid = Set[Coord]

  def isOn(grid: Grid, c: Coord): Boolean = grid.contains(c)

  def parseLine(l: String, r: Int): Set[Coord] =
    l.zipWithIndex
      .collect { case (v, c) if v == On => Coord(r, c) }
      .toSet

  def readFile(f: String): Grid =
    io.Source.fromFile(f)
      .getLines()
      .zipWithIndex
      .map { case (l, r) =>  parseLine(l.trim, r) }
      .flatten
      .toSet

  val inputFile = "data/Day18.txt"
}

package advent

object Day06 {

  def day06(): Unit = {
    val actions = readFile(inputFile)
    println(s"Day06.part1 = ${part1(actions)}")
    println(s"Day06.part2 = ${part2(actions)}")
  }


  def part1(actions: List[Action]): Int = {
    val result = actions.foldLeft(initialGrid) { case (g, a) => applyAction1(g, a) }
    result.size
  }

  def part2(actions: List[Action]): Int = {
    val result = actions.foldLeft(initialGrid) { case (g, a) => applyAction2(g, a) }
    result.values.sum
  }

  def coordsForRegion(start: Coord, end: Coord): Stream[Coord] =
    for {
      r <- (start.row to end.row).toStream
      c <- (start.col to end.col).toStream
    } yield Coord(r, c)

  def applyAction1(grid: Grid, action: Action): Grid = {
    def off(g: Grid, c: Coord): Grid = g - c
    def on(g: Grid, c: Coord): Grid = g + (c -> 1)
    def toggle(g: Grid, c: Coord): Grid =
      if(g.contains(c)) g - c
      else              g + (c -> 1)

    action match {
      case TurnOff(s, e) => coordsForRegion(s, e).foldLeft(grid) { case (g, c) => off(g, c) }
      case TurnOn(s, e) => coordsForRegion(s, e).foldLeft(grid) { case (g, c) => on(g, c) }
      case Toggle(s, e) => coordsForRegion(s, e).foldLeft(grid) { case (g, c) => toggle(g, c) }
    }
  }

  def applyAction2(grid: Grid, action: Action): Grid = {
    def off(g: Grid, c: Coord): Grid = g + (c -> math.max(grid.getOrElse(c, 0) - 1, 0))
    def on(g: Grid, c: Coord): Grid = g + (c -> (g.getOrElse(c, 0) + 1))
    def toggle(g: Grid, c: Coord): Grid = g + (c -> (g.getOrElse(c, 0) + 2))

    action match {
      case TurnOff(s, e) => coordsForRegion(s, e).foldLeft(grid) { case (g, c) => off(g, c) }
      case TurnOn(s, e) => coordsForRegion(s, e).foldLeft(grid) { case (g, c) => on(g, c) }
      case Toggle(s, e) => coordsForRegion(s, e).foldLeft(grid) { case (g, c) => toggle(g, c) }
    }
  }

  type Grid = Map[Coord, Int]

  val initialGrid: Grid = Map.empty[Coord, Int]

  case class Coord(row: Int, col: Int)

  sealed trait Action { val start: Coord; val end: Coord }
  case class TurnOff(start: Coord, end: Coord) extends Action
  case class TurnOn(start: Coord, end: Coord)  extends Action
  case class Toggle(start: Coord, end: Coord)  extends Action

  val turnOffRegex = """turn off (\d+),(\d+) through (\d+),(\d+)""".r
  val turnOnRegex  = """turn on (\d+),(\d+) through (\d+),(\d+)""".r
  val toggleRegex  = """toggle (\d+),(\d+) through (\d+),(\d+)""".r

  def parseAction(s: String): Action =
    s match {
      case turnOffRegex(r1, c1, r2, c2) => TurnOff(Coord(r1.toInt, c1.toInt), Coord(r2.toInt, c2.toInt))
      case turnOnRegex(r1, c1, r2, c2)  => TurnOn(Coord(r1.toInt, c1.toInt), Coord(r2.toInt, c2.toInt))
      case toggleRegex(r1, c1, r2, c2)  => Toggle(Coord(r1.toInt, c1.toInt), Coord(r2.toInt, c2.toInt))
    }

  def readFile(file: String): List[Action] =
    io.Source.fromFile(file)
      .getLines()
      .toList
      .map(parseAction)

  val inputFile = "data/Day06.txt"
}

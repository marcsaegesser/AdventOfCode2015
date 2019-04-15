package advent

object Day03 {

  def day03(): Unit = {
    val input = readFile(inputFile)
    println(s"Day03.part1 = ${part1(input)}")
    println(s"Day03.part2 = ${part2(input)}")
  }

  def part1(input: List[Direction]): Int = {
    val finalWorld = input.foldLeft(initialWorld) { case (w, m) => doMove(m, w) }

    finalWorld.houses.size
  }

  def part2(input: List[Direction]): Int = {
    def splitDirections(input: List[Direction]): (List[Direction], List[Direction]) =
      input.sliding(2, 2).foldLeft((List.empty[Direction], List.empty[Direction])) {
        case ((l1, l2), d1 :: d2 :: Nil) => (d1 :: l1, d2 :: l2)
        case ((l1, l2), d1 :: Nil)       => (d1 :: l1, l2)
        case _                           => throw new Exception("Invalid input")
      }

    val (d1, d2) = splitDirections(input)
    val santa = d1.reverse.foldLeft(initialWorld) { case (w, m) => doMove(m, w) }
    val robo = d2.reverse.foldLeft(initialWorld) { case (w, m) => doMove(m, w) }
    val result = santa.houses ++ robo.houses

    result.size
  }

  def doMove(move: Direction, world: World): World = {
    world match { case World(p, h) =>
      val nextCoord = p.move(move)
      World(nextCoord, h + nextCoord)
    }
  }

  case class Coord(x: Int, y: Int) {
    def move(d: Direction): Coord =
      d match {
        case North => Coord(x,   y-1)
        case East  => Coord(x+1, y)
        case South => Coord(x,   y+1)
        case West  => Coord(x-1, y)
      }
  }

  case class World(p: Coord, houses: Set[Coord])

  val initialWorld = World(Coord(0, 0), Set(Coord(0, 0)))

  sealed trait Direction
  case object North extends Direction
  case object East extends Direction
  case object South extends Direction
  case object West extends Direction

  def readFile(file: String): List[Direction] =
    io.Source.fromFile(file)
      .getLines()
      .toList
      .flatten
      .map(parseDirection)

  def parseDirection(c: Char): Direction =
    c match {
      case '^' => North
      case '>' => East
      case 'v' => South
      case '<' => West
    }


  val inputFile = "data/Day03.txt"
}

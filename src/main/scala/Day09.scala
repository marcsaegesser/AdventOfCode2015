package advent

object Day09 {

  def day09(): Unit = {
    val input = readFile(inputFile)
    println(s"Day09.part1 = ${part1(input)}")
    println(s"Day09.part2 = ${part2(input)}")
  }

  def part1(input: Map[Pair, Path]): Int = {
    val routes = generateRoutes(input)
    routes.sortBy(_._2).head._2
  }

  def part2(input: Map[Pair, Path]): Int = {
    val routes = generateRoutes(input)
    routes.sortBy(_._2).reverse.head._2
  }

  def generateRoutes(input: Map[Pair, Path]): List[(String, Int)] = {
    val cities = input.keys.flatten
    val routes = cities.flatMap(tracksFrom(input, _))
    val routeDist = routes.map { r => r.foldLeft((List.empty[String], 0)) { case ((cs, d), t) =>  (t.city :: cs, d+t.distance)} }

    routeDist.map { case (r, d) => (r.mkString(" -> "), d) }.toList
  }

  def tracksFrom(paths: Map[Pair, Path], from: Place) = {
    def helper(accum: List[List[Track]], paths: Map[Pair, Path]): List[List[Track]] =
      accum.flatMap { p =>
        val (rest, tracks) = findTracks(paths, p.head.city)
        if(tracks.isEmpty) List(p)
        else               helper(tracks.map(t => t :: p), rest)
      }

    helper(List(List(Track(from, 0))), paths)
  }

  def findTracks(paths: Map[Pair, Path], from: Place): (Map[Pair, Path], List[Track]) = {
    val (selected, rest) = paths.partition { case (k, v) => k.contains(from) }
    val tracks = selected.values.map { case Path(cs, d) => Track(cs.filterNot(_ == from).head, d) }.toList
    (rest, tracks)
  }


  type Place = String
  type Pair = Set[Place]
  case class Path(cities: Set[Place], distance: Int)
  case class Track(city: Place, distance: Int)

  val pathRegex = """(\w+) to (\w+) = (\d+)""".r

  def parsePath(s: String): Path =
    s match {
      case pathRegex(p1, p2, d) => Path(Set(p1, p2), d.toInt)
    }

  def readFile(file: String): Map[Pair, Path] =
    io.Source.fromFile(file)
      .getLines()
      .filterNot(_.isEmpty)
      .map { s =>
        val p = parsePath(s)
        (p.cities, p)
      }.toMap

  val inputFile = "data/Day09.txt"
}

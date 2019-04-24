package advent

import scala.math.Integral.Implicits._

object Day14 {

  def day14(): Unit = {
    val input = readFile(inputFile)
    println(s"Day14.part1 = ${part1(input)}")
    println(s"Day14.part2 = ${part2(input)}")
  }

  def part1(input: List[Reindeer]): Int = {
    val results = race(input, 2503)
    results.sortBy(_._2).reverse.head._2
  }

  def part2(input: List[Reindeer]): Int = {
    val results = race2(input, 2503)
    results.sortBy(_._2).reverse.head._2
  }

  def race(reindeer: List[Reindeer], time: Int): List[(Reindeer, Int)] = {
    reindeer
      .map(r => (r, reindeerDistance(r, time)))
      .sortBy(_._2)
      .reverse
  }

  sealed trait ReindeerStatus { def reindeer: Reindeer; def distance: Int }
  case class Flying(reindeer: Reindeer, remaining: Int, distance: Int)  extends ReindeerStatus
  case class Resting(reindeer: Reindeer, remaining: Int, distance: Int) extends ReindeerStatus

  def updateStatus(status: ReindeerStatus): ReindeerStatus = {
    status match {
      case Flying(r, 1, d)     => Resting(r, r.rest, d + r.speed)
      case Flying(r, fly, d)   => Flying(r, fly-1, d + r.speed)
      case Resting(r, 1, d)    => Flying(r, r.fly, d)
      case Resting(r, rest, d) => Resting(r, rest-1, d)
    }
  }

  def mkInitialStatus(reindeer: List[Reindeer]): List[ReindeerStatus] =
    reindeer.map(r =>  Flying(r, r.fly, 0))

  def race2(reindeer: List[Reindeer], time: Int): List[(Reindeer, Int)] = {
    def iterateN(n: Int, accum: Map[String, Int], state: List[ReindeerStatus]): (Map[String, Int], List[ReindeerStatus]) =
      if(n == 0) (accum, state)
      else {
        val updated = state.map(updateStatus).sortBy(_.distance).reverse
        val leaders = updated.takeWhile(_.distance == updated.head.distance)
        val newAccum = leaders.foldLeft(accum) { case (a, s) =>  a + (s.reindeer.name -> (a.getOrElse(s.reindeer.name, 0) + 1))}
        iterateN(n-1, newAccum, updated)
      }

    val (results, status) = iterateN(time, Map.empty[String, Int], mkInitialStatus(reindeer))
    status.map { s => (s.reindeer, results.getOrElse(s.reindeer.name, 0))}
  }

  case class Reindeer(name: String, speed: Int, fly: Int, rest: Int)

  def reindeerDistance(reindeer: Reindeer, duration: Int): Int = {
    val cycleLength = reindeer.fly + reindeer.rest
    val (complete, remaining) = duration /% cycleLength
    val lastFly =
      if(remaining <= reindeer.fly) remaining
      else                         reindeer.fly

    complete * reindeer.speed * reindeer.fly + lastFly * reindeer.speed
  }

  val reindeerRegex = """(\w+) can fly (\d+) km/s for (\d+) seconds, but then must rest for (\d+) seconds.""".r

  def parseReindeer(s: String): Reindeer =
    s match {
      case reindeerRegex(n, s, f, r) => Reindeer(n, s.toInt, f.toInt, r.toInt)
    }

  def readFile(file: String): List[Reindeer] =
    io.Source.fromFile(file)
      .getLines()
      .map(parseReindeer)
      .toList

  val inputFile = "data/Day14.txt"
}

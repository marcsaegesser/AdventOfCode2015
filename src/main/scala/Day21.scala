package advent

import scala.math.Integral.Implicits._

object Day21 {
  case class Player(hp: Int, damage: Int, armor: Int)
  case class Item(name: String, cost: Int, damage: Int, armor: Int)

  def part1(boss: Player): Int = {
    generatePlayersFromStore
      .filter { case (c, p) => player1Wins(p, boss) }
      .sortBy(_._1)
      .head
      ._1
  }

  def part2(boss: Player): Int = {
    generatePlayersFromStore
      .filterNot { case (c, p) => player1Wins(p, boss) }
      .sortBy(_._1)
      .reverse
      .head
      ._1
  }

  def generatePlayersFromStore: List[(Int, Player)] =
    for {
      w <- weapons
      a <- armor
      r <- ringChoices
    } yield (w.cost+a.cost+r.cost, Player(100, w.damage+a.damage+r.damage, w.armor+a.armor+r.armor))


  def turnsToWin(p1: Player, p2: Player): Int = {
    val d = Math.max(p1.damage - p2.armor, 1)

    p2.hp /% d match {
      case (q, 0) => q
      case (q, _) => q+1
    }
  }

  def player1Wins(p1: Player, p2: Player): Boolean = {
    turnsToWin(p1, p2) <= turnsToWin(p2, p1)
  }

  val lineRegex = """(.+):\s+(\d+)""".r

  def parseLine(l: String, prefix: String): Option[Int] = {
    l match {
      case lineRegex(p, v) if p == prefix => Some(v.toInt)
      case _                              => None
    }
  }

  def parsePlayer(lines: List[String]): Player = {
    val hp = lines.map(l => parseLine(l, "Hit Points")).flatten.head
    val damage = lines.map(l => parseLine(l, "Damage")).flatten.head
    val armor = lines.map(l => parseLine(l, "Armor")).flatten.head

    Player(hp, damage, armor)
  }

  def readFile(f: String): Player = {
    parsePlayer(io.Source.fromFile(f).getLines().toList)
  }

  val inputFile = "data/Day21.txt"

  val weapons = List(
    Item("Dagger",        8, 4, 0),
    Item("Shortsword",   10, 5, 0),
    Item("Warhammer",    25, 6, 0),
    Item("Longsword",    40, 7, 0),
    Item("Greataxe",     74, 8, 0)
  )

  val armor = List(
    Item("None",          0, 0, 0),
    Item("Leather",      13, 0, 1),
    Item("Chainmail",    31, 0, 2),
    Item("Splintmail",   53, 0, 3),
    Item("Bandedmail",   75, 0, 4),
    Item("Platemail",   102, 0, 5)
  )

  val rings = List(
    Item("None",          0, 0, 0),
    Item("Damage +1",    25, 1, 0),
    Item("Damage +2",    50, 2, 0),
    Item("Damage +3",   100, 3, 0),
    Item("Defense +1",   20, 0, 1),
    Item("Defense +2",   40, 0, 2),
    Item("Defense +3",   80, 0, 3)
  )

  val ringChoices = Item("None/None", 0, 0, 0) +: rings.combinations(2).map { case List(r1, r2) => Item(s"${r1.name}/${r2.name}", r1.cost+r2.cost, r1.damage+r2.damage, r1.armor+r2.armor)}.toList
}

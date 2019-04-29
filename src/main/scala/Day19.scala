package advent

import scala.collection.parallel.immutable._


object Day19 {

  def part1(puzzle: Puzzle): Int = {
    allReplacements(puzzle.rules, puzzle.medicine).size
  }

  def allReplacements(rules: Rules, input: String): Set[String] =
    splits(input).foldLeft(Set.empty[String]) { case (a, (p, s)) =>
      a ++ applyRules(rules, s).map(p ++ _)
    }

  def runIteration(rules: Rules, strings: ParSet[String]): ParSet[String] =
    strings.flatMap(allReplacements(rules, _))

  def untilFound(rules: Rules, search: String, string: String): (ParSet[String], Int) = {
    def helper(i: Int, accum: ParSet[String]): (ParSet[String], Int) =
      if(accum.contains(search)) (accum, i)
      else                       helper(i+1, runIteration(rules, accum).filter(_.size <= search.size))

    helper(0, ParSet(string))
  }

  def splits(input: String): List[(String, String)] = {
    def helper(accum: List[(String, String)], p: String, s: String): List[(String, String)] =
      if(s.isEmpty) accum
      else          helper((p, s) :: accum, p + s.take(1), s.drop(1))

    helper(List.empty[(String, String)], "", input)
  }

  def applyRules(rules: Rules, input: String): Set[String] =
    rules.foldLeft(Set.empty[String]) { case (a, (r, s)) =>
      if(input.startsWith(r)) a + input.replaceFirst(r, s)
      else                    a
    }

  type Rules = Set[(String, String)]
  case class Puzzle(rules: Rules, medicine: String)

  val ruleRegex = """(\w+) => (\w+)""".r
  val moleculeREgex = """(\w+)""".r

  def parseRules(lines: Iterator[String]): Rules =
    lines.map { case ruleRegex(r, s) => r -> s }.toSet

  def readFile(f: String): Puzzle = {
    val (rules, rest) =
      io.Source.fromFile(f)
        .getLines()
        .filterNot(_.isEmpty)
        .partition(_.contains("=>"))

    Puzzle(parseRules(rules), rest.next)
  }

  val inputFile = "data/Day19.txt"
}

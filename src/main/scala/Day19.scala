package advent

// import scala.annotation.tailrec

object Day19 {

  def part1(puzzle: Puzzle): Int = {
    allReplacements(puzzle.rules, puzzle.medicine).size
  }

  def allReplacements(rules: Rules, input: String): Set[String] =
    splits(input).foldLeft(Set.empty[String]) { case (a, (p, s)) =>
      a ++ applyRules(rules, s).map(p ++ _)
    }

  def runIteration(rules: Rules, strings: Set[String]): Set[String] =
    strings.flatMap(allReplacements(rules, _))

  def untilFound(rules: Rules, search: String, string: String): (Set[String], Int) = {
    def helper(i: Int, accum: Set[String]): (Set[String], Int) =
      if(accum.contains(search)) (accum, i)
      else                       helper(i+1, runIteration(rules, accum).filter(_.size <= search.size))

    helper(0, Set(string))
  }

  def untilFound2(rules: Rules, search: String, start: String) = {
    def helper(i: Int, strings: Set[String]): Option[Int] = {
      if(strings.contains(search)) Some(i)
      else if(strings.isEmpty) None
      else {
        helper(i+1, allReplacements(rules, strings.head).filter(_.size <= search.size)).orElse(helper(i, strings.tail))
      }
    }

    helper(1, allReplacements(rules, start))
  }

  def part2(puzzle: Puzzle): Int = {
    val invRules = reverseRules(puzzle.rules)

    // @tailrec
    def helper(i: Int, strings: Set[String]): Option[Int] = {
      println(s"helper: $i - strings.size=${strings.size}")
      if(strings.contains("e")) Some(i)
      else if(strings.isEmpty) None
      else {
        // val nextStrings =
        //   strings
        //     .flatMap(s => allReplacements(invRules, s).filterNot(s => s.size > 1 && s.contains('e')))

        // helper(i+1, nextStrings)
        val nextStrings =
          allReplacements(invRules, strings.head).filterNot(s => s.size > 1 && s.contains('e'))
        helper(i+1, nextStrings).orElse(helper(i, strings.tail))
      }
    }

    helper(0, Set(puzzle.medicine)).getOrElse(throw new Exception("No solution!"))
  }

  def untilFound3(rules: Rules, search: String, start: String) = {
    def helper(i: Int, strings: Set[String]): Option[Int] = {
      if(strings.contains(search)) Some(i)
      else if(strings.isEmpty) None
      else {
        helper(i+1, allReplacements(rules, strings.head)).orElse(helper(i, strings.tail))
      }
    }

    helper(1, allReplacements(rules, start))
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

  def reverseRules(rules: Rules): Rules =
    rules.map(_.swap)

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

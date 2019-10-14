package advent

import scala.collection.immutable.SortedSet

object Day19 {

  def day19(): Unit = {
    val puzzle = readFile(inputFile)
    println(s"Day19.part1 = ${part1(puzzle)}")
    println(s"Day19.part2 = ${part2(puzzle)}")
  }

  def part1(puzzle: Puzzle): Int =
    applyAllRules(puzzle.rules, puzzle.medicine).size

  def part2(puzzle: Puzzle): Int = {
    val numMolecules = puzzle.medicine.size
    val numEnds = puzzle.medicine.filter(m => m == "Rn" || m == "Ar").size
    val numSeps = puzzle.medicine.filter(_ == "Y").size

    numMolecules - numEnds - 2*numSeps - 1
  }

  type Token    = String
  type Tokens   = Vector[Token]
  type Rule     = (Tokens, Tokens)
  type Rules    = Set[Rule]

  case class Puzzle(rules: Rules, medicine: Tokens)

  case class Node(tokens: Tokens, f: Int) // f = g + h, the estimated cost of reaching the goal from token

  implicit object NodeOrdering extends Ordering[Node] {
    def compare(a: Node, b: Node) = {
      val fs = a.f compare b.f
      if(fs == 0) a.tokens.mkString compare b.tokens.mkString
      else       fs
    }
  }

  def containsTokens(nodes: SortedSet[Node], search: Tokens): Boolean =
    nodes.exists(_.tokens == search)

  def removeNodeIfLonger(nodes: SortedSet[Node], node: Node): SortedSet[Node] =
    nodes.find(_.tokens == node.tokens) match {
      case Some(Node(_, c)) if node.f < c => nodes - node
      case Some(Node(_, _))               => nodes
      case None                           => nodes
    }

  /* A truly shitty A* implementation. Unused because the
   * puzzle is really a context-free grammar so a path search
   * isn't necessary.
   */
  case class AStarState(open: SortedSet[Node], closed: SortedSet[Node], g: Map[Tokens, Int])

  def h(current: Tokens, goal: Tokens): Int =
    goal.diff(current).size

  def part2Bad(puzzle: Puzzle): Int = {
    def iterate(state: AStarState): Int = {
      if(state.open.isEmpty) throw new Exception("Uh oh.")

      val (current, rest) = (state.open.head, state.open.tail)
      // println(s"current=$current")
      if(current.tokens == puzzle.medicine)
        state.g(current.tokens)
      else {
        val closed = state.closed + current
        val neighbors = applyAllRules(puzzle.rules, current.tokens)
        val cost = state.g(current.tokens) + 1
        val (nextOpen, nextClosed, nextG) =
          neighbors.filter(_.size <= puzzle.medicine.size).foldLeft((rest, closed, state.g)) { case ((o, c, g), n) =>
            val gn = g.getOrElse(n, Int.MaxValue)
            val no = removeNodeIfLonger(o, Node(n, gn))
            val nc = removeNodeIfLonger(c, Node(n, gn))
            val (ng, nno) =
              if(!containsTokens(no, n) && !containsTokens(nc, n)) {
                (g + ((n, cost)), no + Node(n, cost + h(n, puzzle.medicine)))
              } else (g, no)
            (nno, nc, ng)
          }
        iterate(AStarState(nextOpen, nextClosed, nextG))
      }
    }

    iterate(AStarState(SortedSet(Node(Vector("e"), 0)), SortedSet.empty[Node], Map(Vector("e") -> 0)))
  }

  def applyAllRules(rules: Rules, input: Tokens): Set[Tokens] =
    rules.foldLeft(Set.empty[Tokens]) { case (a, r) =>
      a ++ applyRule(r, input)
    }

  def applyRule(rule: Rule, input: Tokens): Set[Tokens] = {
    val (l, r) = rule

    def replaceAt(n: Int): Tokens = {
      val (p, s) = input.splitAt(n)
      p ++ r ++ s.drop(l.size)
    }

    def helper(accum: Set[Tokens], n: Int): Set[Tokens] = {
      input.indexOfSlice(l, n) match {
        case -1 => accum
        case n  => helper(accum + replaceAt(n), n+1)
      }
    }

    helper(Set.empty[Tokens], 0)
  }

  val regexElectron = """(e)(.*)""".r
  val regexA        = """([A-Z])(.*)""".r
  val regexAA       = """([A-Z])([A-Z].*)""".r
  val regexAa       = """([A-Z][a-z])(.*)""".r
  val regexEmpty    = """""".r

  def splitString(s: String): Tokens = {
    def helper(accum: List[String], rest: String): List[String] =
      rest match {
        case ""                  => accum.reverse
        case regexElectron(h, t) => helper(h +: accum, t)
        case regexAA(h, t)       => helper(h +: accum, t)
        case regexAa(h, t)       => helper(h +: accum, t)
        case regexA(h, t)        => helper(h +: accum, t)
      }

    helper(List.empty[String], s).toVector
  }

  val ruleRegex = """(\w+) => (\w+)""".r

  def parseRules(lines: Iterator[String]): Rules =
    lines.map { case ruleRegex(r, s) => splitString(r) -> splitString(s) }.toSet

  def readFile(f: String): Puzzle = {
    val (rules, rest) =
      io.Source.fromFile(f)
        .getLines()
        .filterNot(_.isEmpty)
        .partition(_.contains("=>"))

    Puzzle(parseRules(rules), splitString(rest.next))
  }

  val inputFile = "data/Day19.txt"
}

package advent

import scala.collection.immutable.WrappedString

object Day11 {

  def day11(): Unit = {
    println(s"Day11.part1 = ${part1(puzzleInput)}")
    println(s"Day11.part2 = ${part2(puzzleInput)}")
  }

  def part1(input: Password): String =
    nextValidPassword(input)

  def part2(input: Password): String =
    nextValidPassword(nextValidPassword(input))

  def incrementChar(c: Char): (Char, Boolean) = {
    if(c == 'z') ('a', true)
    else        ((c+1).toChar, false)
  }

  def incrementPassword(password: Password): Password = {
    def helper(p: Password, n: Int): Password = {
      if(n < 0) p
      else {
        val (x, c) = incrementChar(p(n))
        val next = p.updated(n, x)
        if(c) helper(next, n-1)
        else  next
      }
    }

    helper(password, password.size-1)
  }

  def nextValidPassword(password: Password): Password = {
    val next = incrementPassword(password)
    if(isValidPassword(next)) next
    else                      nextValidPassword(next)
  }

  def isValidPassword(password: Password): Boolean =
    containsStraight(password) && !containsIOL(password) && containsPairs(password)

  def containsStraight(password: Password): Boolean = {
    def isRun(s: WrappedString): Boolean = s(1) == s(0)+1 && s(2) == s(0) + 2

    password.toSeq.sliding(3).exists(isRun)
  }

  def containsIOL(password: Password): Boolean =
    password.exists(c => "iol".contains(c))

  def containsPairs(password: Password): Boolean = {
    def helper(accum: List[String], s: String): List[String] =
      if(s.isEmpty) accum
      else {
        val (h, t) = s.span(_ == s.head)
        if(h.size == 2) helper(t :: accum, t)
        else           helper(accum, t)
      }

    helper(List.empty[String], password).toSet.size >= 2
  }

  type Password = String

  val puzzleInput = "vzbxkghb"
}

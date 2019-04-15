package advent

import java.security.MessageDigest

object Day04 {

  def day04(): Unit = {
    println(s"Day04.part1 = ${part1(key, 0)}")
    println(s"Day04.part2 = ${part2(key, 0)}")
  }

  val md = MessageDigest.getInstance("MD5")

  def part1(key: String, i: Long): Long = {
    if(testValue(key, i, 5)) i
    else                     part1(key, i+1)
  }

  def part2(key: String, i: Long): Long = {
    if(testValue(key, i, 6)) i
    else                     part2(key, i+1)
  }

  def testValue(key: String, i: Long, size: Int): Boolean = {
    val s = key + i.toString
    val dig = md.digest(s.getBytes).map(b => f"$b%02d").mkString
    dig.take(size).filter(_ == '0').size == size
  }

  val key = "yzbqklnj"
}

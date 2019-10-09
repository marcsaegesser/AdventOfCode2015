package advent

import scala.annotation.tailrec
import scala.math.Integral.Implicits._

object Day20 {

  def part1(n: Int): Int = {
    val target = n/10
    LazyList.from(1).map(i => (i, sumOfFactors(i))).dropWhile(_._2 < target).head._1
  }


  def primes = 2 #:: LazyList.from(3,2).filter(isPrime)

  def isPrime(n: Int): Boolean =
    primes.takeWhile(i => i*i <= n).forall(i => n % i != 0)

  def primeFactors(n: Int): Map[Int, Int] = {
    def helper(accum: List[Int], ps: LazyList[Int], i: Int): Map[Int, Int] =
      if(isPrime(i))            (i :: accum).groupBy(identity).view.mapValues(_.size).toMap
      else if(i % ps.head == 0) helper(ps.head :: accum, ps, i / ps.head)
      else                      helper(accum, ps.tail, i)

    helper(List.empty[Int], primes, n)
  }

  def sumOfFactors(n: Int): Int = {
    def pow(a: Int, b: Int): Int = List.fill(b)(a).product

    def computeTerm(p: Int, n: Int): Int =
      LazyList.from(0).takeWhile(_ <= n).map(pow(p, _)).sum

    primeFactors(n).toList.map((computeTerm _).tupled).product
  }

  // def numPresents(n: Int): Int =
  //   factors(n).sum * 10

  // def factors(n: Int): Set[Int] = {
  //   def helper(accum: Set[Int], i: Int, max: Int): Set[Int] =
  //     if(i > max) accum
  //     else {
  //       println(s"$i, ${n /% i}")
  //       n /% i match {
  //         case (q, r) if r == 0 => helper(accum ++ Set(i, q), i+1, max)
  //         case (q, r)           => helper(accum, i+1, max)
  //       }
  //     }

  //   helper(Set(n), ((n-1)/50)+1, n / 2)
  // }

  def part1a(n: Int): Int = {
    val target = n / 10

    def helper(known: Map[Int, Set[Int]], i: Int): Int = {
      if((i % 1000) == 0) println(s"$i")
      val (f, k) = factors(known, i)
      if(f.sum > target) i
      else               helper(k, i+1)
    }

    helper(Map.empty[Int, Set[Int]], 2)
  }

  def factors(known: Map[Int, Set[Int]], n: Int): (Set[Int], Map[Int, Set[Int]]) = {
    def helper(accum: Set[Int], i: Int): Set[Int] =
      if(i == 1) accum
      else if(accum.contains(i)) helper(accum, i-1)
      else {
        known.get(i) match {
          case Some(fs) => helper(accum ++ fs, i-1)
          case None     =>
            n /% i match {
              case (q, r) if r == 0 => helper(accum ++ Set(i, q), i-1)
              case (_, _)          => helper(accum, i-1)
            }
        }
      }

    val factors = helper(Set(1, n), n/2)
    (factors, known + (n -> factors))
  }

  case class State(elf: Int, houses: Map[Int, Int], matches: Set[Int])

  // This is a horrible brute force approach, but it got the right answer.
 def part2(limit: Int): State = {
    @tailrec
    def helper(state: State): State = {
      state match { case State(elf, houses, matches) =>
        val nextValues =
          LazyList.from(elf, elf)
            .map(h => (h, houses.get(h).getOrElse(0) + elf*11))
            .take(50)
        val nextMatches = matches ++ nextValues.collect { case (h, v) if v >= limit => h }
        val nextHouses = nextValues.foldLeft(houses) { case (ss, (h, v)) => ss.updated(h, v) }
        val nextState = State(elf+1, nextHouses, nextMatches)
        if(!nextMatches.isEmpty) println(s"$elf - ${nextMatches.min}")
        if(!nextMatches.isEmpty && nextMatches.min <= elf) nextState
        else                                             helper(nextState)
      }
    }

    helper(State(1, Map.empty[Int, Int], Set.empty[Int]))
  }

  val puzzleInput = 34000000
}

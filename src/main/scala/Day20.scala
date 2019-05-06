package advent

import scala.math.Integral.Implicits._

object Day20 {

  def part1(n: Int): Int = {
    val target = n/10
    Stream.from(1).map(i => (i, sumOfFactors(i))).dropWhile(_._2 < target).head._1
  }

  def primes = 2 #:: Stream.from(3,2).filter(isPrime)

  def isPrime(n: Int): Boolean =
    primes.takeWhile(i => i*i <= n).forall(i => n % i != 0)

  def primeFactors(n: Int): Map[Int, Int] = {
    def helper(accum: List[Int], ps: Stream[Int], i: Int): Map[Int, Int] =
      if(isPrime(i))            (i :: accum).groupBy(identity).mapValues(_.size)
      else if(i % ps.head == 0) helper(ps.head :: accum, ps, i / ps.head)
      else                      helper(accum, ps.tail, i)

    helper(List.empty[Int], primes, n)
  }

  def sumOfFactors(n: Int): Int = {
    def pow(a: Int, b: Int): Int = List.fill(b)(a).product

    def computeTerm(p: Int, n: Int): Int =
      Stream.from(0).takeWhile(_ <= n).map(pow(p, _)).sum

    primeFactors(n).toList.map((computeTerm _).tupled).product
  }

  def numPresents(n: Int): Int =
    factors(n).sum * 10

  def factors(n: Int): Set[Int] = {
    def helper(accum: Set[Int], i: Int, max: Int): Set[Int] =
      if(i > max) accum
      else {
        println(s"$i, ${n /% i}")
        n /% i match {
          case (q, r) if r == 0 => helper(accum ++ Set(i, q), i+1, max)
          case (q, r)           => helper(accum, i+1, max)
        }
      }

    helper(Set(n), ((n-1)/50)+1, n / 2)
  }


  val puzzleInput = 34000000
}

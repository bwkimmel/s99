package ca.eandb.s99

import collection.immutable.Stream._

/**
 * Created with IntelliJ IDEA.
 * User: brad
 * Date: 5/31/12
 * Time: 7:43 AM
 * To change this template use File | Settings | File Templates.
 */

case class S99Int(n: Int) {

  import S99Int._
  import scala.math._

  def divides(m: Int): Boolean = (m % n == 0)

  /** P31 */
  def isPrime = (n > 1) && (primes takeWhile (_ < sqrt(n)) forall (n % _ != 0))

  /** P33 */
  def isCoprimeTo(m: Int) = (gcd(n, m) == 1)

  /** P34 */
  def totient = (1 to n) count isCoprimeTo

  /** P35 */
  def primeFactors: List[Int] =
    if (n > 1) {
      val p = (primes find (_ divides n) get)
      p :: (n / p).primeFactors
    } else Nil

  /** P36 */
  def primeFactorMultiplicity = P10.encode(primeFactors) map (_.swap)

  /** P37 */
  def totientImproved = {
    def phi(factors: List[(Int, Int)], acc: Int = 1): Int = factors match {
      case (p, 1) :: rest => phi(rest, (p - 1) * acc)
      case (p, m) :: rest => phi((p, m - 1) :: rest, p * acc)
      case Nil => acc
    }
    phi(primeFactorMultiplicity)
  }

}

object S99Int {

  implicit def s99Int2Int(obj: S99Int): Int = obj.n
  implicit def int2s99Int(n: Int): S99Int = S99Int(n)

  val primes: Stream[Int] = cons(2, from(3, 2) filter (_.isPrime))

  /** P32 */
  def gcd(a: Int, b: Int): Int =
    if (a < b)
      gcd(a, b - a)
    else if (a > b)
      gcd(a - b, b)
    else a

}

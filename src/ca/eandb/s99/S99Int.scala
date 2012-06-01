package ca.eandb.s99

/**
 * Created with IntelliJ IDEA.
 * User: brad
 * Date: 5/31/12
 * Time: 7:43 AM
 * To change this template use File | Settings | File Templates.
 */

case class S99Int(n: Int) {

  import S99Int._

  def divides(m: Int): Boolean = (m % n == 0)

  /** P31 */
  def isDivisibleByAtMost(k: Int): Boolean =
    if (k > 1)
      (k divides n) || isDivisibleByAtMost(k - 1)
    else false

  def isPrime = !isDivisibleByAtMost(n - 1)

}

object S99Int {

  implicit def s99Int2Int(obj: S99Int): Int = obj.n
  implicit def int2s99Int(n: Int): S99Int = S99Int(n)

}

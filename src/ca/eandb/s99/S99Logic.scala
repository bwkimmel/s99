package ca.eandb.s99

/**
 * Created with IntelliJ IDEA.
 * User: brad
 * Date: 6/1/12
 * Time: 11:10 PM
 * To change this template use File | Settings | File Templates.
 */

case class S99Logic(a: Boolean) {

  /** P47 */
  def nand(b: Boolean) = !(a && b)
  def and(b: Boolean) = a && b
  def or(b: Boolean) = a || b
  def nor(b: Boolean) = !(a || b)
  def xor(b: Boolean) = a ^ b
  def impl(b: Boolean) = !a || b
  def equ(b: Boolean) = a == b

}

object S99Logic {

  import Util._
  
  implicit def boolean2S99Logic(a: Boolean) = S99Logic(a)
  implicit def s99Logic2Boolean(logic: S99Logic) = logic.a

  def nand(a: Boolean, b: Boolean) = !(a && b)
  def and(a: Boolean, b: Boolean) = a && b
  def or(a: Boolean, b: Boolean) = a || b
  def nor(a: Boolean, b: Boolean) = !(a || b)
  def xor(a: Boolean, b: Boolean) = a ^ b
  def impl(a: Boolean, b: Boolean) = !a || b
  def equ(a: Boolean, b: Boolean) = a == b
  def not(a: Boolean) = !a

  /** P46 */
  def table2(f: (Boolean, Boolean) => Boolean) = {
    println("A     B     result")
    val t = List(true, false)
    cartesian(t :: t :: Nil) collect {
      case a :: b :: Nil => "%-5s %-5s %-5s".format(a, b, f(a, b))
    } foreach println
  }

  /** P49 */
  def gray(n: Int): List[String] = {
    def grayBool(n: Int): List[List[Boolean]] = n match {
      case 1 => List(List(false), List(true))
      case _ => grayBool(n - 1) flatMap { x =>
        List((false xor x.head) :: x, (true xor x.head) :: x)
      }
    }
    grayBool(n) map (_.reverse map (if (_) '1' else '0')) map (_ mkString)
  }

}

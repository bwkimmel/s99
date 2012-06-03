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
    cartesian(t, t) collect {
      case (a, b) => "%-5s %-5s %-5s".format(a, b, f(a, b))
    } foreach println
  }

  /** P49 */
  def gray(n: Int): List[String] = n match {
    case 1 => "0" :: "1" :: Nil
    case _ =>
      val tail = gray(n - 1)
      (tail map ('0' +)) ::: (tail.reverse map ('1' +))
  }

  /** P50 */
  def huffman[T](freq: List[(T, Int)]) = {
    case class Freq(tree: Any, weight: Int) extends Ordered[Freq] {
      override def compare(that: Freq) = weight compare that.weight
    }
    def build(heap: Heap[Freq]): Any =
      if (heap.size > 1) {
        val heap1 = heap.dequeue
        val heap2 = heap1.dequeue
        val a = heap.top
        val b = heap1.top
        build(heap2 + Freq((a.tree, b.tree), a.weight + b.weight))
      } else heap.top.tree
    def codes(tree: Any, prefix: String = "", acc: List[(T, String)] = Nil): List[(T, String)] =
      tree match {
        case (l, r) => codes(l, prefix :+ '0', codes(r, prefix :+ '1', acc))
        case x: T => (x, prefix) :: acc
      }
    val heap = Heap.Empty ++ (freq map (x => Freq(x._1, x._2)))
    codes(build(heap))
  }

  def huffman(lengths: List[Int], acc: List[String] = Nil): List[String] = {
    implicit def seq2String(s: Seq[Char]): String = s.mkString
    def incr(seed: String): (String, Boolean) = (seed: Seq[Char]) match {
      case Seq() => ("", true)
      case Seq(c, rest @ _*) => incr(rest) match {
        case (tail, false) => (c +: tail, false)
        case (tail, true) if c == '1' => ('0' +: tail, true)
        case (tail, true) if c == '0' => ('1' +: tail, false)
      }
    }
    (lengths, acc) match {
      case (n :: rest, Nil) =>
        huffman(rest, "0" * n :: Nil)
      case (n :: rest, seed :: _) => incr(seed) match {
        case (next, false) => huffman(rest, next.padTo(n, '0') :: acc)
        case (_, true) =>
          throw new IllegalArgumentException("Invalid Huffman code lengths")
      }
      case (Nil, _) => acc reverse
    }
  }

}

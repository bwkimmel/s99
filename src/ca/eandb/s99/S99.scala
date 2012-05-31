package ca.eandb.s99

/**
 * Created with IntelliJ IDEA.
 * User: brad
 * Date: 5/30/12
 * Time: 12:09 AM
 * To change this template use File | Settings | File Templates.
 */

import scala.util.Random
import ca.eandb.s99.Common._

object Common {

  def isEqual[T](a: List[T], b: List[T]): Boolean = (a, b) match {
    case (x :: resta, y :: restb) if x == y => isEqual(resta, restb)
    case (Nil, Nil) => true
    case _ => false
  }

}

object P01 {

  def last[T](list: List[T]): T = list match {
    case x :: Nil => x
    case x :: rest => last(rest)
    case _ => sys.error("list is empty")
  }

}

object P02 {

  def penultimate[T](list: List[T]): T = list match {
    case x :: _ :: Nil => x
    case x :: rest => penultimate(rest)
    case _ => sys.error("not enough elements")
  }

}

object P03 {

  def nth[T](index: Int, list: List[T]): T =
    if (index > 0)
      nth(index - 1, list.tail)
    else list head

}

object P04 {

  def length[T](list: List[T], acc: Int = 0): Int = list match {
    case Nil => acc
    case _ :: rest => length(rest, acc + 1)
  }

}

object P05 {

  def reverse[T](list: List[T], acc: List[T] = Nil): List[T] = list match {
    case Nil => acc
    case x :: rest => reverse(rest, x :: acc)
  }

}

object P06 {

  def isPalindrome[T](list: List[T]) =
    isEqual(list, P05.reverse(list))

}

object P07 {

  def flatten[T](list: List[Any], acc: List[Any] = Nil): List[Any] = list match {
    case Nil => P05.reverse(acc)
    case (x: List[Any]) :: rest => flatten(rest, flatten(x) ::: acc)
    case x :: rest => flatten(rest, x :: acc)
  }

}

object P08 {

  def compress[T](list: List[T], acc: List[T] = Nil): List[T] = list match {
    case Nil => P05.reverse(acc)
    case x :: (tail @ (y :: rest)) if x == y => compress(tail, acc)
    case x :: rest => compress(rest, x :: acc)
  }

}

object P09 {

  def pack[T](list: List[T], acc: List[List[T]] = Nil): List[List[T]] = list match {
    case Nil => P05.reverse(acc)
    case x :: (tail @ (y :: rest)) if x != y =>
      pack(tail, (x :: Nil) :: acc)
    case x :: rest =>
      pack(rest, (x :: acc.head) :: acc.tail)
  }

}

object P10 {

  def incr[T] = (i: Int, x: T) => (i + 1, x)

  def encode[T](list: List[T], acc: List[(Int, T)] = Nil) =
    P09.pack(list).map(x => (P04.length(x), x.head))

}

object P11 {

  def incr[T](e: Any) = e match {
    case x: T => (2, x)
    case (i: Int, x: T) => (i + 1, x)
  }

  def encode[T](list: List[T], acc: List[Any] = Nil): List[Any] = list match {
    case Nil => P05.reverse(acc)
    case x :: (tail @ (y :: rest)) if x != y =>
      encode(tail, x :: acc)
    case x :: rest =>
      encode(rest, incr(acc.head) :: acc.tail)
  }

}

object P12 {

  def decode[T](list: List[(Int, T)], acc: List[T] = Nil): List[T] = list match {
    case Nil => P05.reverse(acc)
    case (i, x) :: rest if i > 0 => decode((i - 1, x) :: rest, x :: acc)
    case _ :: rest => decode(rest, acc)
  }

}

object P13 {

  def incr[T](x: (Int, T)) = (x._1 + 1, x._2)

  def encodeDirect[T](list: List[T], acc: List[(Int, T)] = Nil): List[(Int, T)] = list match {
    case Nil => P05.reverse(acc)
    case x :: (tail @ (y :: rest)) if x != y =>
      encodeDirect(tail, (1, x) :: acc)
    case x :: rest =>
      encodeDirect(rest, incr(acc.head) :: acc.tail)
  }

}

object P14 {

  def duplicate[T](list: List[T]): List[T] =
    P12.decode(list map ((2, _)))

}

object P15 {

  def duplicateN[T](n: Int, list: List[T]): List[T] =
    P12.decode(list map ((n, _)))

}

object P16 {

  private def drop[T](n: Int, list: List[T], j: Int, acc: List[T]): List[T] =
    (j, list) match {
      case (_, Nil) => P05.reverse(acc)
      case (0, x :: rest) => drop(n, rest, n - 1, acc)
      case (_, x :: rest) => drop(n, rest, j - 1, x :: acc)
    }

  def drop[T](n: Int, list: List[T]): List[T] = drop(n, list, n - 1, Nil)

}

object P17 {

  def split[T](index: Int, list: List[T], acc: List[T] = Nil): (List[T], List[T]) =
    if (index > 0)
      split(index - 1, list.tail, list.head :: acc)
    else
      (P05.reverse(acc), list)

}

object P18 {

  def slice[T](i: Int, j: Int, list: List[T], acc: List[T] = Nil): List[T] =
    if (i > 0)
      slice(i - 1, j - 1, list.tail, acc)
    else if (j > 0)
      slice(0, j - 1, list.tail, list.head :: acc)
    else
      P05.reverse(acc)

}

object P19 {

  def rotate[T](n: Int, list: List[T]) =
    ((a: List[T], b: List[T]) => b ::: a) tupled P17.split(n, list)

}

object P20 {

  def removeAt[T](index: Int, list: List[T]) = P17.split(index, list) match {
    case (a, x :: b) => (a ::: b, x)
    case _ => sys.error("index out of bounds")
  }

}

object P21 {

  def insertAt[T](e: T, index: Int, list: List[T]) = P17.split(index, list) match {
    case (a, b) => a ::: e :: b
  }

}

object P22 {

  def range(i: Int, j: Int, acc: List[Int] = Nil): List[Int] =
    if (i < j)
      range(i, j - 1, j :: acc)
    else acc

}

object P23 {

  def randomSelect[T](n: Int, list: List[T], rnd: Random = new Random, acc: List[T] = Nil): List[T] = {
    val index = rnd.nextInt(P04.length(list))
    val rem = P20.removeAt(index, list)
    if (n > 0)
      randomSelect(n - 1, rem._1, rnd, rem._2 :: acc)
    else acc
  }

}

object P24 {

  def lotto(n: Int, m: Int) =
    P23.randomSelect(n, P22.range(1, m))

}

object P25 {

  def randomPermute[T](list: List[T]) =
    P23.randomSelect(P04.length(list), list)

}

object P26 {

  def combination[T](n: Int, list: List[T]): List[List[T]] = (n, list) match {
    case (0, _) | (_, Nil) => Nil
    case (n, x :: rest) =>
      (combination(n - 1, rest) map (x :: _)) ::: combination(n, rest)
  }

}
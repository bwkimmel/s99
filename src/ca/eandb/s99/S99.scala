package ca.eandb.s99

/**
 * Created with IntelliJ IDEA.
 * User: brad
 * Date: 5/30/12
 * Time: 12:09 AM
 * To change this template use File | Settings | File Templates.
 */

import scala.util.Random
import java.util.NoSuchElementException

import Util._

object P01 {

  def last[T](list: List[T]): T = list match {
    case x :: Nil => x
    case x :: rest => last(rest)
    case _ => throw new NoSuchElementException("last of empty list")
  }

}

object P02 {

  def penultimate[T](list: List[T]): T = list match {
    case x :: _ :: Nil => x
    case x :: rest => penultimate(rest)
    case _ => throw new NoSuchElementException("penultimate of list of < 2 elements")
  }

}

object P03 {

  def nth[T](index: Int, list: List[T]): T = (index, list) match {
    case (0, x :: _) => x
    case (_, _ :: rest) => nth(index - 1, rest)
  }

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

  def pack[T](list: List[T], acc: List[List[T]] = Nil): List[List[T]] =
    (list, acc) match {
      case (Nil, _) => P05.reverse(acc)
      case (x :: rest, (head @ (y :: _)) :: tail) if x == y =>
        pack(rest, (x :: head) :: tail)
      case (x :: rest, _) =>
        pack(rest, (x :: Nil) :: acc)
    }

}

object P10 {

  def encode[T](list: List[T]) =
    P09.pack(list).map(x => (P04.length(x), x.head))

}

object P11 {

  def encodeModified[T](list: List[T]): List[Any] =
    P10.encode(list).map { _ match {
      case (1, x) => x
      case pair => pair
    }}

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

  def encodeDirect[T](list: List[T], acc: List[(Int, T)] = Nil): List[(Int, T)] =
    (list, acc) match {
      case (Nil, _) => P05.reverse(acc)
      case (x :: rest, (n, y) :: tail) if x == y =>
        encodeDirect(rest, (n + 1, y) :: tail)
      case (x :: rest, _) =>
        encodeDirect(rest, (1, x) :: acc)
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
    P17.split(j - i, P17.split(i, list)._2)._1

}

object P19 {

  def rotate[T](n: Int, list: List[T]) =
    ((a: List[T], b: List[T]) => b ::: a) tupled P17.split(
      if (n < 0) P04.length(list) + n else n, list)

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
    if (i <= j)
      range(i, j - 1, j :: acc)
    else acc

}

object P23 {

  def randomSelect[T](n: Int, list: List[T], rnd: Random = new Random, acc: List[T] = Nil): List[T] =
    if (n > 0) {
      val index = rnd.nextInt(P04.length(list))
      val rem = P20.removeAt(index, list)
      randomSelect(n - 1, rem._1, rnd, rem._2 :: acc)
    } else acc

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

  def combinations[T](n: Int, list: List[T]): List[List[T]] = (n, list) match {
    case (0, _) => Nil :: Nil
    case (_, Nil) => Nil
    case (_, x :: rest) =>
      (combinations(n - 1, rest) map (x :: _)) ::: combinations(n, rest)
  }

}

object P27 {

  def partitions[T](n: Int, list: List[T]): List[(List[T], List[T])] = (n, list) match {
    case (0, _) => (Nil, list) :: Nil
    case (_, Nil) => Nil
    case (_, x :: rest) =>
      (partitions(n - 1, rest) map { case (a, b) => (x :: a, b) }) :::
        (partitions(n, rest) map { case (a, b) => (a, x :: b) })
  }

  def group[T](n: List[Int], list: List[T]): List[List[List[T]]] =
    n match {
      case Nil => Nil :: Nil
      case n1 :: rest =>
        partitions(n1, list) flatMap {
          case (a, b) => group(rest, b) map (a :: _) } }

  def group3[T](list: List[T]) = group(List(3, 3, 3), list)

}

object P28 {

  def lsort[T](lists: List[List[T]]): List[List[T]] =
    lists.zip(lists map { x => P04.length(x) }).sortBy(_._2).map(_._1)

  def lsortFreq[T](lists: List[List[T]]): List[List[T]] = {
    val lengths = lists map { x => P04.length(x) }
    val freq = Map(P10.encode(lengths sorted) map (_.swap):_*)
    (lists zip lengths sortBy { case (_, len) => freq(len) }).map(_._1)
  }

}
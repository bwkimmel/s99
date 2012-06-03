package ca.eandb.s99
package tree.binary

/**
 * Created with IntelliJ IDEA.
 * User: brad
 * Date: 6/3/12
 * Time: 9:29 AM
 * To change this template use File | Settings | File Templates.
 */

import Util._

sealed abstract class Tree[+T]

case class Node[+T](value: T, left: Tree[T], right: Tree[T]) extends Tree[T] {
  override def toString = "T(%s %s %s)".format(value, left, right)
}

case object Leaf extends Tree[Nothing] {
  override def toString = "."
}

object Node {
  def apply[T](value: T): Node[T] = Node(value, Leaf, Leaf)
}

object Tree {

  /** P55 */
  def cBalanced[T](n: Int, value: T): List[Tree[T]] = n match {
    case 0 => Leaf :: Nil
    case 1 => Node(value) :: Nil
    case TwicePlusOne(k) =>
      val ts = cBalanced(k, value)
      cartesian(ts :: ts :: Nil) flatMap {
        case l :: r :: Nil => Node(value, l, r) :: Nil
      }
    case Twice(k) =>
      cartesian(cBalanced(k, value) :: cBalanced(k - 1, value) :: Nil) flatMap {
        case l :: r :: Nil => Node(value, l, r) :: Node(value, r, l) :: Nil
      }
  }

}

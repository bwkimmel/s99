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

sealed abstract class Tree[+T] {

  def isMirrorOf(t: Tree[Any]): Boolean = (this, t) match {
    case (Node(_, a1, b1), Node(_, b2, a2)) =>
      (a1 isMirrorOf a2) && (b1 isMirrorOf b2)
    case (Leaf, Leaf) => true
    case _ => false
  }

  /** P56 */
  def isSymmetric = this match {
    case Leaf => true
    case Node(_, a, b) => a isMirrorOf b
  }

  /** P57 */
  def addValue[U >: T <% Ordered[U]](value: U): Tree[U] = (value, this) match {
    case (x, Leaf) => Node(x)
    case (x, n @ Node(y, _, _)) if x == y => n
    case (x, Node(y, l, r)) if x < y => Node(y, l.addValue(x), r)
    case (x, Node(y, l, r)) => Node(y, l, r.addValue(x))
  }

}

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
      cartesian(ts, ts) flatMap {
        case (l, r) => Node(value, l, r) :: Nil
      }
    case Twice(k) =>
      cartesian(cBalanced(k, value), cBalanced(k - 1, value)) flatMap {
        case (l, r) => Node(value, l, r) :: Node(value, r, l) :: Nil
      }
  }

  /** P57 (Part 2) */
  def fromList[T <% Ordered[T]](list: List[T]): Tree[T] =
    (Leaf.asInstanceOf[Tree[T]] /: list)(_ addValue _)

  /** P58 */
  def symmetricBalancedTrees[T](n: Int, value: T) =
    cBalanced(n, value) filter (_ isSymmetric)

  /** P59 */
  def hbalTrees[T](h: Int, value: T): List[Tree[T]] = h match {
    case 0 => Leaf :: Nil
    case 1 => Node(value) :: Nil
    case _ =>
      val t0 = hbalTrees(h - 1, value)
      val t1 = hbalTrees(h - 2, value)
      val bal = cartesian(t0, t0) map {
        case (a, b) => Node(value, a, b)
      }
      val lop = cartesian(t0, t1) flatMap {
        case (a, b) => Node(value, a, b) :: Node(value, b, a) :: Nil
      }
      bal ::: lop
  }

}

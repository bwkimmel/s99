package ca.eandb.s99
package tree.binary

/**
 * Created with IntelliJ IDEA.
 * User: brad
 * Date: 6/3/12
 * Time: 9:29 AM
 * To change this template use File | Settings | File Templates.
 */

import scala.math._
import Util._

sealed abstract class Tree[+T] {

  def shift(dx: Int, dy: Int) = this

  def size: Int = this match {
    case End => 0
    case Node(_, a, b) => 1 + a.size + b.size
  }

  def height: Int = this match {
    case End => 0
    case Node(_, a, b) => 1 + max(a height, b height)
  }

  def isHeightBalanced: Boolean = this match {
    case End => true
    case Node(_, a, b) => a.isHeightBalanced && b.isHeightBalanced &&
      abs(a.height - b.height) < 2
  }

  def isMirrorOf(t: Tree[Any]): Boolean = (this, t) match {
    case (Node(_, a1, b1), Node(_, b2, a2)) =>
      (a1 isMirrorOf a2) && (b1 isMirrorOf b2)
    case (End, End) => true
    case _ => false
  }

  /** P56 */
  def isSymmetric = this match {
    case End => true
    case Node(_, a, b) => a isMirrorOf b
  }

  /** P57 */
  def addValue[U >: T <% Ordered[U]](value: U): Tree[U] = (value, this) match {
    case (x, End) => Node(x)
    case (x, n @ Node(y, _, _)) if x == y => n
    case (x, Node(y, l, r)) if x < y => Node(y, l.addValue(x), r)
    case (x, Node(y, l, r)) => Node(y, l, r.addValue(x))
  }

  /** P61 */
  def leafCount: Int = this match {
    case End => 0
    case Node(_, End, End) => 1
    case Node(_, a, b) => a.leafCount + b.leafCount
  }

  /** P61A */
  private def rake[U >: T](acc: List[U] = Nil): List[U] = this match {
    case End => acc
    case Node(x, End, End) => x :: acc
    case Node(_, a, b) => a rake (b rake acc)
  }
  def leafList = rake()

  /** P62 */
  private def collectInternal[U >: T](acc: List[U] = Nil): List[U] = this match {
    case End | Node(_, End, End) => acc
    case Node(x, a, b) => x :: (a collectInternal (b collectInternal acc))
  }
  def internalList = collectInternal()

  /** P62B */
  def atLevel[U >: T](n: Int, acc: List[U] = Nil): List[U] = (n, this) match {
    case (1, Node(x, _, _)) => x :: acc
    case (_, Node(_, a, b)) => a.atLevel(n - 1, b.atLevel(n - 1, acc))
    case _ => acc
  }

  /** P64 */
  private def layoutBinaryTree(x: Int, y: Int): (Tree[T], Int) = this match {
    case End => (End, 0)
    case Node(value, left, right) =>
      val (lp, lw) = left.layoutBinaryTree(x, y + 1)
      val (rp, rw) = right.layoutBinaryTree(x + lw + 1, y + 1)
      (PositionedNode(value, lp, rp, x + lw, y), 1 + lw + rw)
  }
  def layoutBinaryTree: Tree[T] = layoutBinaryTree(1, 1)._1

  /** P65 */
  private def layoutBinaryTree2(x: Int, y: Int, dx: Int): Tree[T] = this match {
    case End => End
    case Node(value, left, right) =>
      val lp = left.layoutBinaryTree2(x - dx, y + 1, dx / 2)
      val rp = right.layoutBinaryTree2(x + dx, y + 1, dx / 2)
      PositionedNode(value, lp, rp, x, y)
  }
  def layoutBinaryTree2: Tree[T] = {
    val h = height
    val lh = (iterate(this) (_ match {
      case Node(_, l, _) => Some(l)
      case _ => None
    })).length
    val x = powi(2, h - 1) - powi(2, h - lh - 1)
    layoutBinaryTree2(x, 1, powi(2, h - 2))
  }

  /** P66 */
  private def layoutSpacing(left: List[(Int, Int)], right: List[(Int, Int)]): Int =
    (left, right) match {
      case (_, Nil) | (Nil, _) => 1
      case ((_, lmax) :: ltail, (rmin, _) :: rtail) =>
        max(((lmax - rmin) + 2) / 2, layoutSpacing(ltail, rtail))
    }
  private def mergeRanges(dx: Int, left: List[(Int, Int)], right: List[(Int, Int)]): List[(Int, Int)] =
    (left, right) match {
      case (Nil, Nil) => Nil
      case ((lmin, lmax) :: ltail, Nil) =>
        (lmin - dx, lmax - dx) :: mergeRanges(dx, ltail, Nil)
      case (Nil, (rmin, rmax) :: rtail) =>
        (rmin + dx, rmax + dx) :: mergeRanges(dx, Nil, rtail)
      case ((lmin, lmax) :: ltail, (rmin, rmax) :: rtail) =>
        (lmin - dx, rmax + dx) :: mergeRanges(dx, ltail, rtail)
    }
  private def layoutBinaryTree3WithRange(): (Tree[T], List[(Int, Int)]) = this match {
    case End => (End, Nil)
    case Node(value, left, right) =>
      val (lp, lr) = left.layoutBinaryTree3WithRange()
      val (rp, rr) = right.layoutBinaryTree3WithRange()
      val dx = layoutSpacing(lr, rr)
      (PositionedNode(value, lp.shift(-dx, 1), rp.shift(dx, 1), 0, 1),
        (0, 0) :: mergeRanges(dx, lr, rr))
  }
  def layoutBinaryTree3: Tree[T] = {
    val (layout, ranges) = layoutBinaryTree3WithRange()
    val x0 = ranges.unzip._1.min
    layout.shift(1 - x0, 0)
  }

}

case class Node[+T](value: T, left: Tree[T], right: Tree[T]) extends Tree[T] {
  override def toString = "T(%s %s %s)".format(value, left, right)
}

case class PositionedNode[+T](
                               override val value: T,
                               override val left: Tree[T],
                               override val right: Tree[T],
                               x: Int, y: Int)
  extends Node[T](value, left, right) {

  override def shift(dx: Int, dy: Int) =
    PositionedNode(value, left.shift(dx, dy), right.shift(dx, dy), x + dx, y + dy)

  override def toString = "T[%d,%d](%s %s %s)".format(x, y, value, left, right)
}

case object End extends Tree[Nothing] {
  override def toString = "."
}

object Node {
  def apply[T](value: T): Node[T] = Node(value, End, End)
}

object Tree {

  /** P55 */
  def cBalanced[T](n: Int, value: T): List[Tree[T]] = n match {
    case 0 => End :: Nil
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
    (End.asInstanceOf[Tree[T]] /: list)(_ addValue _)

  /** P58 */
  def symmetricBalancedTrees[T](n: Int, value: T) =
    cBalanced(n, value) filter (_ isSymmetric)

  /** P59 */
  def hbalTrees[T](h: Int, value: T): List[Tree[T]] = h match {
    case 0 => End :: Nil
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

  /** P60 */
  def minHbalNodes(h: Int, acc: (Int, Int) = (0, 0)): Int = (h, acc) match {
    case (0, (n, _)) => n
    case (_, (n, m)) => minHbalNodes(h - 1, (1 + n + m, n))
  }

  def maxHbalHeight(n: Int) = {
    def next(x: (Int, Int)): (Int, Int) = x match { case (n, m) => (1 + n + m, n) }
    iterate((0, 0))(next).zipWithIndex.collectFirst {
      case ((k, _), h) if k > n => h - 1
    } get
  }

  def maxHbalNodes(h: Int, acc: Int = 1): Int = h match {
    case 0 => acc - 1
    case _ => maxHbalNodes(h - 1, acc * 2)
  }

  def minHbalHeight(n: Int, acc: Int = 0): Int = n match {
    case 0 => acc
    case _ => minHbalHeight(n / 2, acc + 1)
  }

  def hbalTreesWithNodes[T](n: Int, value: T): List[Tree[T]] =
    minHbalHeight(n) to maxHbalHeight(n) flatMap (hbalTrees(_, value)) filter (_.size == n) toList

  /** P63 */
  private def completeBinaryTree[T](h: Int, r: Int, value: T): Tree[T] = (h, r) match {
    case (0, _) | (1, 0) => End
    case _ =>
      val a: Int = min(r, powi(2, h - 2))
      val b: Int = r - a
      Node(value,
        completeBinaryTree(h - 1, a, value),
        completeBinaryTree(h - 1, b, value))
  }

  def completeBinaryTree[T](n: Int, value: T): Tree[T] = {
    val h = minHbalHeight(n)
    val r = n - maxHbalNodes(h - 1)
    completeBinaryTree(h, r, value)
  }

}

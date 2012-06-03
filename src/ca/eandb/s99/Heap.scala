package ca.eandb.s99

import collection.GenTraversableOnce
import Util._

/**
 * Created with IntelliJ IDEA.
 * User: brad
 * Date: 6/2/12
 * Time: 12:33 PM
 * To change this template use File | Settings | File Templates.
 */

sealed abstract class Heap[+T <% Ordered[T]] {

  def size: Int

  def ++[T1 >: T <% Ordered[T1]](that: GenTraversableOnce[T1]): Heap[T1] = that match {
    case Nil => this
    case x :: rest => (this + x) ++ rest
  }

  def +[T1 >: T <% Ordered[T1]](value: T1): Heap[T1] = this match {
    case Heap.empty => Heap(value)
    case Heap(x, left, right) if left.size < right.size =>
      Heap(x, left + value, right) match {
        case Heap(y, Heap(z, a, b), r) if z < y => Heap(z, Heap(y, a, b), r)
        case h => h
      }
    case Heap(x, left, right) =>
      Heap(x, left, right + value) match {
        case Heap(y, l, Heap(z, a, b)) if z < y => Heap(z, l, Heap(y, a, b))
        case h => h
      }
  }

  def top: T = this match {
    case Heap(value, _, _) => value
  }

  def dequeue: Heap[T] = this match {
    case Heap(_, Heap.empty, Heap.empty) => Heap.empty
    case Heap(value, Heap(x, a, b), h @ Heap(y, _, _)) if x < y =>
      Heap(x, Heap(value, a, b) dequeue, h)
    case Heap(value, h @ Heap(x, _, _), Heap(y, a, b)) =>
      Heap(y, h, Heap(value, a, b) dequeue)
    case Heap(_, h @ Heap(_, _, _), Heap.empty) => h
    case Heap(_, Heap.empty, h @ Heap(_, _, _)) => h
  }

  def toStream = unfold(this) { _ match {
    case h @ Heap(x, _, _) => Some((x, h dequeue))
    case _ => None
  } }

  def toList = toStream.toList

}

object Heap {

  final case class Internal[T <% Ordered[T]](value: T, left: Heap[T], right: Heap[T])
    extends Heap[T] {
    val size = 1 + left.size + right.size
  }

  object empty extends Heap[Nothing] {
    override def toString = "Leaf"
    val size = 0
  }

  def apply[T <% Ordered[T]](value: T): Heap[T] = Heap(value, Heap.empty, Heap.empty)
  def apply[T <% Ordered[T]](value: T, left: Heap[T], right: Heap[T]): Heap[T] =
    Internal(value, left, right)

  def unapply[T <% Ordered[T]](heap: Heap[T]) = heap match {
    case Internal(value, left, right) => Some((value, left, right))
    case _ => None
  }

}

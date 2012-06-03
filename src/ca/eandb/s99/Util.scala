package ca.eandb.s99

/**
 * Created with IntelliJ IDEA.
 * User: brad
 * Date: 6/1/12
 * Time: 8:11 AM
 * To change this template use File | Settings | File Templates.
 */

object Util {

  def isEqual[T](a: List[T], b: List[T]): Boolean = (a, b) match {
    case (x :: resta, y :: restb) if x == y => isEqual(resta, restb)
    case (Nil, Nil) => true
    case _ => false
  }

  def cartesian[T](lists: List[List[T]]): List[List[T]] = lists match {
    case x :: rest => cartesian(rest) flatMap (z => x map (_ :: z))
    case Nil => Nil :: Nil
  }

  def unfold[A, B](x: A)(f: A => Option[(B, A)]): Stream[B] = f(x) match {
    case Some((e, y)) => Stream.cons(e, unfold(y)(f))
    case None => Stream.empty
  }

  object Twice {
    def apply(n: Int) = 2 * n
    def unapply(n: Int) = if (n % 2 == 0) Some(n / 2) else None
  }

  object TwicePlusOne {
    def apply(n: Int) = 2 * n + 1
    def unapply(n: Int) = if (n % 2 == 1) Some((n - 1) / 2) else None
  }

}

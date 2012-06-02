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

}

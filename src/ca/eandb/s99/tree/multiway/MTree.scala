package ca.eandb.s99
package tree.multiway

/**
 * Created with IntelliJ IDEA.
 * User: brad
 * Date: 6/4/12
 * Time: 12:49 AM
 * To change this template use File | Settings | File Templates.
 */

case class MTree[+T](value: T, children: List[MTree[T]]) {
  def this(value: T) = this(value, List())
  override def toString = "M(" + value.toString + " {" + children.map(_.toString).mkString(",") + "})"
}

object MTree {
  def apply[T](value: T) = new MTree(value, List())
  def apply[T](value: T, children: List[MTree[T]]) = new MTree(value, children)
}

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

  /** P70C */
  def nodeCount: Int = 1 + children.map(_.nodeCount).sum

  /** P70 */
  //  override def toString = "M(" + value.toString + " {" + children.map(_.toString).mkString(",") + "})"
  override def toString =
    value + children.mkString + "^"

  /** P71 */
  def internalPathLength: Int =
    children.map(c => c.nodeCount + c.internalPathLength).sum

}

object MTree {

  def apply[T](value: T) = new MTree(value, Nil)

  /** P70 */
  def parseMTreeList(s: List[Char], acc: List[MTree[Char]] = Nil): (List[MTree[Char]], List[Char]) =
    s match {
      case '^' :: rest => (acc.reverse, rest)
      case _ =>
        val (tree, rest) = parseMTree(s)
        parseMTreeList(rest, tree :: acc)
    }

  def parseMTree(s: List[Char]): (MTree[Char], List[Char]) =
    s match {
      case Nil => throw new IllegalArgumentException("Invalid node string")
      case value :: r0 =>
        val (children, r1) = parseMTreeList(r0)
        (MTree(value, children), r1)
    }

  implicit def string2MTree(s: String) = parseMTree(s.toList) match {
    case (tree, Nil) => tree
    case _ => throw new IllegalArgumentException("Invalid node string")
  }

}

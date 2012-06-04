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

  /** P72 */
  def postorder: List[T] = children.flatMap(_.postorder) :+ value

  /** P73 */
  def lispyTree: String = children match {
    case Nil => value.toString
    case _ => "(%s %s)".format(value, children.map(_.lispyTree).mkString(" "))
  }

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

  private val openp = """^\s*\(\s*(\w+)\b\s*(.*)$""".r
  private val closep = """^\s*\)\s*(.*)$""".r
  private val word = """^\s*(\w+)\b\s*(.*)$""".r

  /** P73 */
  def parseLispMTreeList(s: String, acc: List[MTree[String]] = Nil): (List[MTree[String]], String) =
    s match {
      case closep(rest) => (acc.reverse, rest)
      case _ =>
        val (child, rest) = parseLispMTree(s)
        parseLispMTreeList(rest, child :: acc)
    }

  def parseLispMTree(s: String): (MTree[String], String) = s match {
    case word(value, rest) => (MTree(value), rest)
    case openp(value, rest) =>
      val (children, rest1) = parseLispMTreeList(rest)
      (MTree(value, children), rest1)
    case _ => throw new IllegalArgumentException("Parse error")
  }

  def fromLispExpr(s: String): MTree[String] = parseLispMTree(s) match {
    case (tree, "") => tree
    case _ => throw new IllegalArgumentException("Parse error")
  }


}

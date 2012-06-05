package ca.eandb.s99
package graph

/**
 * Created with IntelliJ IDEA.
 * User: brad
 * Date: 6/4/12
 * Time: 7:37 PM
 * To change this template use File | Settings | File Templates.
 */

abstract class GraphBase[T, U] {
  case class Edge(n1: Node, n2: Node, value: U) {
    def toTuple = (n1.value, n2.value, value)
  }
  case class Node(value: T) {
    var adj: List[Edge] = Nil
    // neighbors are all nodes adjacent to this node.
    def neighbors: List[Node] = adj.map(edgeTarget(_, this).get)
  }

  var nodes: Map[T, Node] = Map()
  var edges: List[Edge] = Nil

  // If the edge E connects N to another node, returns the other node,
  // otherwise returns None.
  def edgeTarget(e: Edge, n: Node): Option[Node]

  override def equals(o: Any) = o match {
    case g: GraphBase[_,_] => (nodes.keys.toList -- g.nodes.keys.toList == Nil &&
      edges.map(_.toTuple) -- g.edges.map(_.toTuple) == Nil)
    case _ => false
  }
  def addNode(value: T) = {
    val n = new Node(value)
    nodes = Map(value -> n) ++ nodes
    n
  }

  /** P80 */
  def toTermForm: (List[T], List[(T,T,U)]) =
    (nodes.keys.toList, edges.map(_.toTuple))

  /** P80 */
  def toAdjacentForm: List[(T, List[(T,U)])] =
    nodes.map { case (t, n) =>
      (t, n.adj.flatMap( e =>
        edgeTarget(e, n).map( n => (n.value, e.value) )))
    }.toList

  /** P81 */
  def findPaths(src: T, dst: T): List[List[T]] = {
    val from = nodes(src)
    val to = nodes(dst)
    def search(here: Node, visited: Set[Node] = Set.empty): List[List[T]] =
      if (here == to)
        List(to.value :: Nil)
      else if (visited(here))
        Nil
      else
        here.neighbors.flatMap(search(_, visited + here).map(here.value :: _))
    search(from)
  }

}

class Graph[T, U] extends GraphBase[T, U] {
  override def equals(o: Any) = o match {
    case g: Graph[_,_] => super.equals(g)
    case _ => false
  }

  def edgeTarget(e: Edge, n: Node): Option[Node] =
    if (e.n1 == n) Some(e.n2)
    else if (e.n2 == n) Some(e.n1)
    else None

  def addEdge(n1: T, n2: T, value: U) = {
    val e = new Edge(nodes(n1), nodes(n2), value)
    edges = e :: edges
    nodes(n1).adj = e :: nodes(n1).adj
    nodes(n2).adj = e :: nodes(n2).adj
  }

  /** P80 */
  override def toString =
    (edges.map(e => "%s-%s".format(e.n1.value, e.n2.value)) ++
      nodes.collect { case (t, n) if n.adj.isEmpty => t.toString }).mkString(
        "[", ", ", "]")

}

class Digraph[T, U] extends GraphBase[T, U] {
  override def equals(o: Any) = o match {
    case g: Digraph[_,_] => super.equals(g)
    case _ => false
  }

  def edgeTarget(e: Edge, n: Node): Option[Node] =
    if (e.n1 == n) Some(e.n2)
    else None

  def addArc(source: T, dest: T, value: U) = {
    val e = new Edge(nodes(source), nodes(dest), value)
    edges = e :: edges
    nodes(source).adj = e :: nodes(source).adj
  }

  /** P80 */
  override def toString = {
    val isolated = nodes.keySet --
      edges.flatMap(e => List(e.n1.value, e.n2.value)).toSet
    (edges.map(e => "%s>%s/%s".format(e.n1.value, e.n2.value, e.value)) :::
      isolated.toList.map(_.toString)).mkString("[", ", ", "]")
  }
}

abstract class GraphObjBase {
  type GraphClass[T, U]
  def addLabel[T](edges: List[(T, T)]) =
    edges.map(v => (v._1, v._2, ()))
  def term[T](nodes: List[T], edges: List[(T,T)]) =
    termLabel(nodes, addLabel(edges))
  def termLabel[T, U](nodes: List[T], edges: List[(T,T,U)]): GraphClass[T, U]
  def addAdjacentLabel[T](nodes: List[(T, List[T])]) =
    nodes.map(a => (a._1, a._2.map((_, ()))))
  def adjacent[T](nodes: List[(T, List[T])]) =
    adjacentLabel(addAdjacentLabel(nodes))
  def adjacentLabel[T, U](nodes: List[(T, List[(T,U)])]): GraphClass[T, U]

  protected val outer = """^\s*\[\s*\b([^\]]*)\b\s*]\s*$""".r
  protected val isolatedNode = """^\s*(\w+)\s*(.*)$""".r
  protected val directedLabeledEdge = """^\s*(\w+)\s*>\s*(\w+)\s*/\s*(\d+)\s*(.*)$""".r
  protected val undirectedLabeledEdge = """^\s*(\w+)\s*-\s*(\w+)\s*/\s*(\d+)\s*(.*)$""".r
  protected val directedEdge = """^\s*(\w+)\s*>\s*(\w+)\s*(.*)$""".r
  protected val undirectedEdge = """^\s*(\w+)\s*-\s*(\w+)\s*(.*)$""".r
  protected val comma = """^\s*,\s*(.*)$""".r
}

object Graph extends GraphObjBase {
  type GraphClass[T, U] = Graph[T, U]

  def termLabel[T, U](nodes: List[T], edges: List[(T,T,U)]) = {
    val g = new Graph[T, U]
    nodes.map(g.addNode)
    edges.map(v => g.addEdge(v._1, v._2, v._3))
    g
  }
  def adjacentLabel[T, U](nodes: List[(T, List[(T,U)])]) = {
    val g = new Graph[T, U]
    for ((v, a) <- nodes) g.addNode(v)
    for ((n1, a) <- nodes; (n2, l) <- a) {
      if (!g.nodes(n1).neighbors.contains(g.nodes(n2)))
        g.addEdge(n1, n2, l)
    }
    g
  }

  /** P80 */
  private def parseItemListTail(s: String, acc: Graph[String, Unit]): Graph[String, Unit] =
    s match {
      case comma(rest) => parseItemList(rest, acc)
      case "" => acc
      case _ => throw new IllegalArgumentException("Parse error")
    }
  private def parseItemList(s: String, acc: Graph[String, Unit]): Graph[String, Unit] =
    s match {
      case "" => acc
      case undirectedEdge(n1, n2, rest) =>
        acc.addNode(n1); acc.addNode(n2)
        acc.addEdge(n1, n2, ())
        parseItemListTail(rest, acc)
      case isolatedNode(n, rest) =>
        acc.addNode(n)
        parseItemListTail(rest, acc)
      case _ => throw new IllegalArgumentException("Parse error")
    }
  def fromString(s: String): Graph[String, Unit] =
    s match {
      case outer(inner) => parseItemList(inner, new Graph[String, Unit])
      case _ => throw new IllegalArgumentException("Parse error")
    }
  private def parseLabeledItemListTail(s: String, acc: Graph[String, Int]): Graph[String, Int] =
    s match {
      case comma(rest) => parseLabeledItemList(rest, acc)
      case "" => acc
      case _ => throw new IllegalArgumentException("Parse error")
    }
  private def parseLabeledItemList(s: String, acc: Graph[String, Int]): Graph[String, Int] =
    s match {
      case "" => acc
      case undirectedLabeledEdge(n1, n2, label, rest) =>
        acc.addNode(n1); acc.addNode(n2)
        acc.addEdge(n1, n2, label.toInt)
        parseLabeledItemListTail(rest, acc)
      case isolatedNode(n, rest) =>
        acc.addNode(n)
        parseLabeledItemListTail(rest, acc)
      case _ => throw new IllegalArgumentException("Parse error")
    }
  def fromStringLabel(s: String): Graph[String, Int] =
    s match {
      case outer(inner) => parseLabeledItemList(inner, new Graph[String, Int])
      case _ => throw new IllegalArgumentException("Parse error")
    }

}

object Digraph extends GraphObjBase {
  type GraphClass[T, U] = Digraph[T, U]

  def termLabel[T, U](nodes: List[T], edges: List[(T,T,U)]) = {
    val g = new Digraph[T, U]
    nodes.map(g.addNode)
    edges.map(v => g.addArc(v._1, v._2, v._3))
    g
  }
  def adjacentLabel[T, U](nodes: List[(T, List[(T,U)])]) = {
    val g = new Digraph[T, U]
    for ((n, a) <- nodes) g.addNode(n)
    for ((s, a) <- nodes; (d, l) <- a) g.addArc(s, d, l)
    g
  }

  /** P80 */
  private def parseItemListTail(s: String, acc: Digraph[String, Unit]): Digraph[String, Unit] =
    s match {
      case comma(rest) => parseItemList(rest, acc)
      case "" => acc
      case _ => throw new IllegalArgumentException("Parse error")
    }
  private def parseItemList(s: String, acc: Digraph[String, Unit]): Digraph[String, Unit] =
    s match {
      case "" => acc
      case directedEdge(src, dst, rest) =>
        acc.addNode(src); acc.addNode(dst)
        acc.addArc(src, dst, ())
        parseItemListTail(rest, acc)
      case undirectedEdge(n1, n2, rest) =>
        acc.addNode(n1); acc.addNode(n2)
        acc.addArc(n1, n2, ())
        acc.addArc(n2, n1, ())
        parseItemListTail(rest, acc)
      case isolatedNode(n, rest) =>
        acc.addNode(n)
        parseItemListTail(rest, acc)
      case _ => throw new IllegalArgumentException("Parse error")
    }
  def fromString(s: String): Digraph[String, Unit] =
    s match {
      case outer(inner) => parseItemList(inner, new Digraph[String, Unit])
      case _ => throw new IllegalArgumentException("Parse error")
    }
  private def parseLabeledItemListTail(s: String, acc: Digraph[String, Int]): Digraph[String, Int] =
    s match {
      case comma(rest) => parseLabeledItemList(rest, acc)
      case "" => acc
      case _ => throw new IllegalArgumentException("Parse error")
    }
  private def parseLabeledItemList(s: String, acc: Digraph[String, Int]): Digraph[String, Int] =
    s match {
      case "" => acc
      case directedLabeledEdge(src, dst, label, rest) =>
        acc.addNode(src); acc.addNode(dst)
        acc.addArc(src, dst, label.toInt)
        parseLabeledItemListTail(rest, acc)
      case undirectedLabeledEdge(n1, n2, label, rest) =>
        acc.addNode(n1); acc.addNode(n2)
        acc.addArc(n1, n2, label.toInt)
        acc.addArc(n2, n1, label.toInt)
        parseLabeledItemListTail(rest, acc)
      case isolatedNode(n, rest) =>
        acc.addNode(n)
        parseLabeledItemListTail(rest, acc)
      case _ => throw new IllegalArgumentException("Parse error")
    }
  def fromStringLabel(s: String): Digraph[String, Int] =
    s match {
      case outer(inner) => parseLabeledItemList(inner, new Digraph[String, Int])
      case _ => throw new IllegalArgumentException("Parse error")
    }

}

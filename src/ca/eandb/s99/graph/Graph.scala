package ca.eandb.s99
package graph

import javax.swing.tree.TreeNode

/**
 * Created with IntelliJ IDEA.
 * User: brad
 * Date: 6/4/12
 * Time: 7:37 PM
 * To change this template use File | Settings | File Templates.
 */

abstract class GraphBase[T, U] {
  protected val nodeSeparator: String

  case class Edge(n1: Node, n2: Node, value: U) {
    def toTuple = (n1.value, n2.value, value)

    /** P80 */
    override def toString = value match {
      case () => "%s%s%s".format(n1, nodeSeparator, n2)
      case _ => "%s%s%s/%s".format(n1, nodeSeparator, n2, value)
    }
  }
  case class Node(value: T) {
    var adj: List[Edge] = Nil
    // neighbors are all nodes adjacent to this node.
    def neighbors: List[Node] = adj.map(edgeTarget(_, this).get)

    def toAdjacentTuple = (value, adj.map(e => (edgeTarget(e, this).get.value, e.value)))

    /** P86 */
    lazy val degree = adj.length

    /** P80 */
    override def toString = value.toString
  }

  var nodes: Map[T, Node] = Map()
  var edges: List[Edge] = Nil

  def nodeCount = nodes.size
  def edgeCount = edges.size

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

  def toGraph: Graph[T, U]
  def graphType: GraphObjBase

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
  protected def search(here: Node, to: Node, visited: Set[Node] = Set.empty): List[List[Edge]] =
    if (here == to)
      List(Nil)
    else
      here.adj.flatMap { e =>
        val next = edgeTarget(e, here).get
        if (visited(next)) Nil
        else search(next, to, visited + here).map(e :: _)
      }

  protected def pathNodes(from: Node, edges: List[Edge]): List[Node] =
    edges.scanLeft(from)((n, e) => edgeTarget(e, n).get)

  def findPaths(from: Node, to: Node): List[List[Node]] =
    search(from, to).map(pathNodes(from, _))

  def findPaths(from: T, to: T): List[List[T]] =
    findPaths(nodes(from), nodes(to)).map(_.map(_.value))

  /** P82 */
  def findCycles(n: T): List[List[T]] =
    nodes(n).neighbors.flatMap(m => findPaths(m.value, n)) collect {
      case path @ (x :: y :: z :: rest) => n :: path
    }

  /** P85 */
  def findHomomorphism[T2, U2](that: GraphBase[T2, U2]): Option[Map[T, T2]] = {
    def build(nodes1: List[T], nodes2: Set[T2], f: Map[T, T2] = Map.empty[T, T2]): Option[Map[T, T2]] =
      nodes1 match {
        case n1 :: rest =>
          nodes2.collectFirst((n2: T2) =>
            build(rest, nodes2 - n2, f + (n1 -> n2)) match {
              case Some(h) if nodes(n1).neighbors.map(n => h(n.value)).toSet.subsetOf(
                that.nodes(n2).neighbors.map(_.value).toSet) => h
            })
        case Nil => Some(f)
      }
    build(nodes.keys.toList, that.nodes.keySet)
  }

  def isIsomorphicTo[T2, U2](that: GraphBase[T2, U2]): Boolean =
    this.findHomomorphism(that).isDefined && that.findHomomorphism(this).isDefined

  /** P86 */
  def nodesByDegree = nodesByDegreeDecreasing
  def nodesByDegreeIncreasing = nodes.values.toList.sortBy(_.degree)
  def nodesByDegreeDecreasing = nodes.values.toList.sortBy(-_.degree)

  def colorNodes: List[(Node, Int)] =
    nodesByDegree.foldLeft(Map.empty[Node, Int])((colors, node) =>
      colors + (node -> Stream.from(1).filterNot(
        node.neighbors.flatMap(colors.get).contains).head)).toList

  /** P87 */
  def nodesByDepthFrom(n: T): List[T] = {
    def visit(remaining: List[Node] = Nil, visited: Set[Node] = Set.empty, acc: List[T] = Nil): List[T] =
      remaining match {
        case n :: rest if !visited(n) => visit(n.neighbors ++ remaining, visited + n, n.value :: acc)
        case n :: rest => visit(rest, visited, acc)
        case Nil => acc
      }
    visit(nodes(n) :: Nil)
  }

  def nodesByDepthPreOrderFrom(n: T): List[T] =
    nodesByDepthFrom(n) reverse

  /** P88 */
  def splitGraph: List[GraphObjBase#GraphClass[T, U]] = {
    val g = toGraph
    def split(remaining: Set[T], acc: List[GraphObjBase#GraphClass[T, U]] = Nil): List[GraphObjBase#GraphClass[T, U]] =
      remaining.headOption match {
        case Some(n) =>
          val componentNodes = g.nodesByDepthFrom(n)
          val component = graphType.adjacentLabel(
            componentNodes.map(nodes(_).toAdjacentTuple))
          split(remaining -- componentNodes, component :: acc)
        case None => acc
      }
    split(nodes.keySet)
  }

  /** P89 */
  def isBipartite = (colorNodes.map(_._2).toSet.size <= 2)

  def findShortestPathLengths(from: T)(implicit numeric: Numeric[U]): Map[T, U] = {
    def search(here: T, din: Map[T, U]): Map[T, U] =
      (nodes(here).adj :\ din) { (e, d) =>
        val next = edgeTarget(e, nodes(here)).get.value
        val dist = numeric.plus(d(here), e.value)
        d.get(next) match {
          case None =>
            search(next, d + (next -> dist))
          case Some(dmin) if numeric.lt(dist, dmin) =>
            search(next, d + (next -> dist))
          case _ => d
        }
      }
    search(from, Map(from -> numeric.zero))
  }

}

class Graph[T, U] extends GraphBase[T, U] {
  override protected val nodeSeparator = "-"

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

  override def toGraph = this
  override def graphType = Graph

  /** P80 */
  override def toString =
    (edges.map(_.toString) ++
      nodes.collect { case (t, n) if n.adj.isEmpty => t.toString }).mkString(
        "[", ", ", "]")

  /** P83 */
  def findSpanningTrees(remaining: Set[Node], visited: Set[Node], edges: List[Edge]): List[Graph[T, U]] = {
    if (remaining isEmpty)
      List(Graph.termLabel(nodes.keys.toList, edges.map(_.toTuple)))
    else
      visited.toList.flatMap(search(_, remaining.head, visited)).flatMap( path =>
        findSpanningTrees(
          remaining -- path.flatMap(e => Seq(e.n1, e.n2)),
          visited   ++ path.flatMap(e => Seq(e.n1, e.n2)),
          edges     ++ path))
  }

  /** P84 */
  def minimalSpanningTree(implicit ordering: Ordering[U]) = {
    def build(g: Graph[T, U])(implicit ordering: Ordering[U]): Option[Graph[T, U]] = {
      def visited(x: (Edge, Node)) = g.nodes.contains(x._2.value)
      val out = g.nodes.keys.toList.flatMap(n =>
        nodes(n).adj.map(e =>
          (e, edgeTarget(e, nodes(n)).get)).filterNot(visited))
      out match {
        case Nil if nodes.size == g.nodes.size => Some(g)
        case Nil => None
        case _ =>
          val (edge, node) = out.minBy(_._1.value)
          g.addNode(node.value)
          g.addEdge(edge.n1.value, edge.n2.value, edge.value)
          build(g)
      }
    }

    val g = new Graph[T, U]
    g.addNode(nodes.keys.head)
    build(g)
  }

  def spanningTrees: List[Graph[T, U]] =
    findSpanningTrees(nodes.values.toSet - nodes.values.head, Set(nodes.values.head), Nil)

  def isTree = spanningTrees.size == 1
  def isConnected = spanningTrees nonEmpty

}

class Digraph[T, U] extends GraphBase[T, U] {
  override protected val nodeSeparator = ">"

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

  override def toGraph = Graph.termLabel(nodes.keys.toList, edges.map(_.toTuple))
  override def graphType = Digraph

  /** P80 */
  override def toString = {
    val isolated = nodes.keySet --
      edges.flatMap(e => List(e.n1.value, e.n2.value)).toSet
    (edges.map(_.toString) ++
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

package ca.eandb.s99

import graph.Graph

/**
 * Created with IntelliJ IDEA.
 * User: brad
 * Date: 6/10/12
 * Time: 11:29 PM
 * To change this template use File | Settings | File Templates.
 */

object P90 {

  def nQueens(n: Int, i: Int = 0, acc: List[Int] = Nil): Option[List[Int]] = {
    def isSafe(j: Int) =
      acc.zipWithIndex.forall{ case (j2, i2) =>
        j != j2 && j != j2 - i2 - 1 && j != j2 + i2 + 1 }
    if (i < n)
      (1 to n).flatMap {
        case j if isSafe(j) => nQueens(n, i + 1, j :: acc)
        case _ => None
      } headOption
    else Some(acc)
  }

  def eightQueens = nQueens(8).get

}

object P91 {

  private val knightJumps = Stream(
    Vector(-1, -2), Vector(-2, -1),
    Vector(-1,  2), Vector(-2,  1),
    Vector( 1, -2), Vector( 2, -1),
    Vector( 1,  2), Vector( 2,  1))

  case class Vector(x: Int, y: Int)
  case class Board(n: Int) {
    case class Point(x: Int, y: Int) {
      def + : PartialFunction[Vector, Point] = {
        case Vector(dx, dy) if (1 to n).contains(x + dx) && (1 to n).contains(y + dy) => Point(x + dx, y + dy)
      }
    }
    def allPoints =
      for (i <- 1 to n iterator; j <- 1 to n iterator)
        yield Point(i, j)
    def allPointsStream = allPoints.toStream
    val size = n * n

    private def findTours(start: Stream[Point]): Stream[List[Point]] = {
      def search(
                  p: Point,
                  visited: Set[Point] = Set.empty,
                  acc: List[Point] = Nil): Stream[List[Point]] = {
        val neighbours = knightJumps.collect(p +).filterNot(visited).sortBy(q =>
          knightJumps.collect(q +).filterNot(visited + p).size)
        if (neighbours nonEmpty)
          neighbours.flatMap(search(_, visited + p, p :: acc))
        else if (visited.size == size - 1)
          Stream(p :: acc)
        else
          Stream.empty
      }
      start.flatMap(search(_))
    }

    def knightsTours = findTours(allPointsStream)
    def knightsToursFrom(p: Point) = findTours(Stream(p))

    def closedKnightsTours =
      knightsToursFrom(Point(1, 1)).find(tour => knightJumps.collect(tour.last +).contains(tour.head))

  }

}

object P92 {

  def findMap[T, U](g: Graph[T, U]): Option[Map[T, Int]] = {
    if (!g.isTree)
      throw new IllegalArgumentException("g not a tree")

    def search(
                nodes: List[T],
                nodeLabels: Map[T, Int],
                availNodeLabels: Set[Int],
                availEdgeLabels: Set[Int]): Option[Map[T, Int]] =
    nodes match {
      case node :: rest =>
        val neighbors = g.nodes(node).neighbors
        val allowedByNeighbours =
          neighbors.map(_.value).flatMap(nodeLabels.get).map(nodeLabel =>
            availEdgeLabels.flatMap(edgeLabel =>
              List(nodeLabel + edgeLabel, nodeLabel - edgeLabel)))
        val allowed = (availNodeLabels /: allowedByNeighbours)(_ & _)
        allowed.toStream.flatMap(nodeLabel =>
          search(
            rest,
            nodeLabels + (node -> nodeLabel),
            availNodeLabels - nodeLabel,
            availEdgeLabels -- neighbors.map(_.value).flatMap(nodeLabels.get).map(
              adjNodeLabel => math.abs(adjNodeLabel - nodeLabel)))).headOption

      case Nil => Some(nodeLabels)
    }

    val nodeLabels = (1 to g.nodeCount) toSet
    val edgeLabels = nodeLabels - g.nodeCount
    search(
      g.nodesByDepthPreOrderFrom(g.nodes.keys.head),
      Map.empty,
      nodeLabels,
      edgeLabels)
  }

}

object P93 {

  case class Eq(lhs: Expr, rhs: Expr) {
    def isTrue = (lhs.value == rhs.value)
    override def toString = "%s = %s".format(lhs, rhs)
  }

  sealed trait Expr {
    def value: Option[Int]
  }
  case class Add(x: Expr, y: Expr) extends Expr {
    lazy val value = (x.value, y.value) match {
      case (Some(a), Some(b)) => Some(a + b)
      case _ => None
    }
    override def toString = "(%s + %s)".format(x, y)
  }
  case class Sub(x: Expr, y: Expr) extends Expr {
    lazy val value = (x.value, y.value) match {
      case (Some(a), Some(b)) => Some(a - b)
      case _ => None
    }
    override def toString = "(%s - %s)".format(x, y)
  }
  case class Mul(x: Expr, y: Expr) extends Expr {
    lazy val value = (x.value, y.value) match {
      case (Some(a), Some(b)) => Some(a * b)
      case _ => None
    }
    override def toString = "(%s * %s)".format(x, y)
  }
  case class Div(x: Expr, y: Expr) extends Expr {
    lazy val value = (x.value, y.value) match {
      case (Some(a), Some(b)) if b != 0 && a % b == 0 => Some(a / b)
      case _ => None
    }
    override def toString = "(%s / %s)".format(x, y)
  }
  case class Const(x: Int) extends Expr {
    val value = Some(x)
    override def toString = x.toString
  }

  def isCanonical(expr: Expr): Boolean = expr match {
    case Add(a, Add(b, c)) => false
    case Mul(a, Mul(b, c)) => false
    case Add(a, Sub(b, c)) => false
    case Mul(a, Div(b, c)) => false
    case Sub(a, b) => isCanonical(a) && isCanonical(b)
    case Div(a, b) => isCanonical(a) && isCanonical(b)
    case _ => true
  }

  def findExpressions(terms: List[Int]): Stream[Expr] = terms match {
    case Nil => Stream.empty
    case x :: Nil => Stream(Const(x))
    case _ =>
      (1 to (terms.length - 1)).toStream.map(terms splitAt _).flatMap {
        case (a, b) =>
          val asub = findExpressions(a)
          val bsub = findExpressions(b)
          asub.flatMap(x => bsub.flatMap(y =>
            List(Add(x, y), Sub(x, y), Div(x, y), Mul(x, y)))) }
  }

  def findCanonicalExpressions(terms: List[Int]): Stream[Expr] =
    findExpressions(terms).filter(isCanonical)

  def findEquations(terms: List[Int]): Stream[Eq] = {
    (1 to (terms.length - 1)).toStream.map(terms splitAt _).flatMap {
      case (a, b) =>
        val lhsByVal = findExpressions(a).filter(_.value.isDefined).groupBy(_.value.get)
        val rhsByVal = findExpressions(b).filter(_.value.isDefined).groupBy(_.value.get)
        lhsByVal.flatMap {
          case (lval, lexprs) =>
            rhsByVal.get(lval).toStream.flatMap(rexprs =>
              lexprs.flatMap(lhs =>
                rexprs.map(rhs => Eq(lhs, rhs)))) } } }

  def findCanonicalEquations(terms: List[Int]): Stream[Eq] =
    findEquations(terms).filter(eq =>
      isCanonical(eq.lhs) && isCanonical(eq.rhs))

}

object P95 {

  def digits(num: Int, acc: List[Int] = Nil): List[Int] = (num, acc) match {
    case (0, Nil) => List(0)
    case (0, _) => acc
    case (n, _) if n > 0 => digits(num / 10, (num % 10) :: acc)
    case _ => throw new IllegalArgumentException("num < 0")
  }

  val digitNames =
    Array("zero", "one", "two"  , "three", "four",
          "five", "six", "seven", "eight", "nine")

  val teens =
    Array("ten"    , "eleven" , "twelve"   , "thirteen", "fourteen",
          "fifteen", "sixteen", "seventeen", "eighteen", "nineteen")

  val tens =
    Array("-"    , "-"    , "twenty" , "thirty", "forty",
          "fifty", "sixty", "seventy", "eighty", "ninety")

  val hundred = "hundred"

  val powersOf1000 =
    Array("", "thousand", "million", "billion", "trillion", "quadrillion",
      "quintillion", "sextillion", "septillion", "octillion", "nonillion",
      "decillion", "undecillion", "duodecillion", "tredecillion",
      "quattuordecillion", "quidecillion", "sexdecillion", "septendecillion",
      "octodecillion", "novemdecillion", "vigintillion")

  def fullWordsSimple(num: Int): String =
    digits(num).map(digitNames).mkString("-")

  def fullWordsTriple(d: (Int, Int, Int)): String = d match {
    case (0, 0, x) => digitNames(x)
    case (0, 1, x) => teens(x)
    case (0, t, 0) => tens(t)
    case (0, t, x) => "%s-%s".format(tens(t), digitNames(x))
    case (h, 0, 0) => List(digitNames(h), hundred).mkString(" ")
    case (h, t, x) =>
      List(digitNames(h), hundred, fullWordsTriple((0, t, x))).mkString(" ")
  }

  def fullWords(num: Int): String =
    fullWords(digits(num))

  def fullWords(d: List[Int]): String =
    d.reverse.grouped(3).map(_.padTo(3, 0)).zipWithIndex.toList.reverse.flatMap {
      case (List(0, 0, 0), _) => Nil
      case (List(x, t, h), 0) => List(fullWordsTriple((h, t, x)))
      case (List(x, t, h), p) =>
        List(fullWordsTriple((h, t, x)), powersOf1000(p))
      case _ => throw new IllegalArgumentException()
    } match {
      case Nil => digitNames(0)
      case words => words.mkString(" ")
    }

}
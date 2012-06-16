package ca.eandb.s99.sudoku

/**
 * Created with IntelliJ IDEA.
 * User: brad
 * Date: 6/14/12
 * Time: 7:40 PM
 * To change this template use File | Settings | File Templates.
 */

/* P97 */

object Sudoku {

  val symbols = ".123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ".toStream

}

case class Sudoku(n: Int = 3) {

  private lazy val symbols = Sudoku.symbols.take(n * n + 1).toArray

  private lazy val all = Stream.from(1).take(n * n).toSet

  case class CellRef(blockRow: Int, blockCol: Int, cellRow: Int, cellCol: Int) {

    def linkedCells: List[CellRef] =
      (1 to n).toList.flatMap(i => (1 to n).flatMap(j => List(
        if (i != blockRow) Some(copy(blockRow = i, cellRow = j)) else None,
        if (i != blockCol) Some(copy(blockCol = i, cellCol = j)) else None,
        if ((i, j) != (cellRow, cellCol))
          Some(copy(cellRow = i, cellCol = j))
        else None) flatten))

  }

  def allCells: Stream[CellRef] =
    (1 to n view).flatMap(br =>
      (1 to n view).flatMap(bc =>
        (1 to n view).flatMap(cr =>
          (1 to n view).map(cc => CellRef(br, bc, cr, cc) )))).toStream

  lazy val numberOfCells = n * n * n * n

  case class Board(cells: Map[CellRef, Int], branches: Map[CellRef, Set[Int]]) {

    private lazy val nextCell =
      branches.filterKeys(!cells.keySet(_)).find(_._2.size == 1)

    override def toString = {
      val strs = cells.mapValues(symbols)

      val lineSep = (1 to n).map(x =>
        (1 to n).map(x => " ").mkString("  ")).mkString("\n", " | ", "\n")
      val blockSep = (1 to n).map(x =>
        (1 to n).map(x => "-").mkString("--")).mkString("\n", "-+-", "\n")

      (1 to n).map(br =>
        (1 to n).map(cr =>
          (1 to n).map(bc =>
            (1 to n).map(cc =>
              strs.getOrElse(CellRef(br, bc, cr, cc), "."))
              .mkString("  "))
            .mkString(" | "))
          .mkString(lineSep))
        .mkString(blockSep)
    }

    def isFinal = nextCell isEmpty

    def isSolved = (cells.size == numberOfCells)

    def isTrapped = branches.exists(_._2.isEmpty)

    def hasUniqueSolution = solution.isSolved

    def advance: Board =
      nextCell match {
        case Some((cell, values)) if values.size == 1 =>
          Board(
            cells + (cell -> values.head),
            cell.linkedCells.foldLeft(branches) {
              case (next, linked) =>
                next + (linked -> (next.getOrElse(linked, all) - values.head))
            })
        case None => this
      }

    def advance(steps: Int): Board = steps match {
      case 0 => this
      case _ => advance.advance(steps - 1)
    }

    def solution: Board =
      if (isFinal) this else advance.solution

    def set(cell: CellRef, value: Int) =
      Board(
        cells + (cell -> value),
        branches + (cell -> (branches.getOrElse(cell, all) & Set(value))) ++
          cell.linkedCells.map(linked =>
            linked -> (branches.getOrElse(linked, all) - value)))

  }

  object Board {

    val empty = Board(Map.empty, Map.empty)

    def apply(cells: Map[CellRef, Int]): Board =
      cells.foldLeft(Board.empty) {
        case (board, (cell, value)) => board.set(cell, value) }

  }

}

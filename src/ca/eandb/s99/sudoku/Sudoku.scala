package ca.eandb.s99.sudoku

/**
 * Created with IntelliJ IDEA.
 * User: brad
 * Date: 6/14/12
 * Time: 7:40 PM
 * To change this template use File | Settings | File Templates.
 */

/* P97 */

case class Sudoku(n: Int) {

  private lazy val symbols = (1 to (n * n)).toSet

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

  case class Board(cells: Map[CellRef, Int], branches: Map[CellRef, Set[Int]] = Map.empty) {

    private lazy val nextCell =
      branches.filterKeys(!cells.keySet(_)).find(_._2.size == 1)

    def isFinal = nextCell isEmpty

    def isSolved = (cells.size == numberOfCells)

    def advance: Board =
      nextCell match {
        case Some((cell, values)) if values.size == 1 =>
          Board(
            cells + (cell -> values.head),
            cell.linkedCells.foldLeft(branches) {
              case (next, linked) =>
                next + (linked -> (next.getOrElse(linked, symbols) - values.head))
            })
        case None => this
      }

    def solve: Board =
      if (isFinal) this else advance.solve

  }

}

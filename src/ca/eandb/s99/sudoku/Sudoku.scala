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

  case class Cell(blockRow: Int, blockCol: Int, cellRow: Int, cellCol: Int) {

    def linkedCells: List[Cell] =
      (1 to n).toList.flatMap(i => (1 to n).flatMap(j => List(
        if (i != blockRow) Some(copy(blockRow = i, cellRow = j)) else None,
        if (i != blockCol) Some(copy(blockCol = i, cellCol = j)) else None,
        if ((i, j) != (cellRow, cellCol))
          Some(copy(cellRow = i, cellCol = j))
        else None) flatten))

  }

  object Cell {
    def apply(row: Int, col: Int): Cell = Cell(
      1 + (row - 1) / n,
      1 + (col - 1) / n,
      1 + (row - 1) % n,
      1 + (col - 1) % n)
  }

  def allCells: Stream[Cell] =
    (1 to n view).flatMap(br =>
      (1 to n view).flatMap(bc =>
        (1 to n view).flatMap(cr =>
          (1 to n view).map(cc => Cell(br, bc, cr, cc) )))).toStream

  lazy val numberOfCells = n * n * n * n

  def reduce(branches: Map[Cell, Set[Int]]): Map[Cell, Set[Int]] = {
    val cellsByRow =
      (1 to (n * n) view).map(row =>
        (1 to (n * n) toList).map(col =>
          Cell(row, col)))
    val cellsByCol =
      (1 to (n * n) view).map(col =>
        (1 to (n * n) toList).map(row =>
          Cell(row, col)))
    val cellsByBlock =
      (1 to n view).flatMap(br =>
        (1 to n view).map(bc =>
          (1 to n toList).flatMap(cr =>
            (1 to n toList).map(cc =>
              Cell(br, bc, cr, cc)))))
    val groups: Seq[List[Cell]] = (cellsByRow ++ cellsByCol ++ cellsByBlock)

    def pruneBranchTuples(branches: Map[Cell, Set[Int]], group: List[Cell]): Map[Cell, Set[Int]] =
      branches ++ (group.groupBy(branches) collect {
        case (values, cells) if cells.size >= values.size =>
          (group -- cells.take(values.size)).map(cell =>
            (cell -> (branches(cell) -- values))) } flatten)

    def pruneSlotTuples(branches: Map[Cell, Set[Int]], group: List[Cell]): Map[Cell, Set[Int]] =
      branches ++ (all.groupBy(value => group.filter(branches(_)(value))) collect {
        case (cells, values) if values.size >= cells.size =>
          cells.map(cell =>
            (cell -> values.take(cells.size))) } flatten)

    def reduceGroup(branches: Map[Cell, Set[Int]], group: List[Cell]): Map[Cell, Set[Int]] =
      pruneSlotTuples(pruneBranchTuples(branches, group), group)

    groups.foldLeft(branches)(reduceGroup)
  }

  case class Board(cells: Map[Cell, Int], branches: Map[Cell, Set[Int]]) {

    private lazy val nextCell =
      allCells.filterNot(cells.keySet).find(branches(_).size == 1).map(cell =>
        (cell, branches(cell).head))

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
              strs.getOrElse(Cell(br, bc, cr, cc), "."))
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
        case Some((cell, value)) =>
          Board(
            cells + (cell -> value),
            reduce(cell.linkedCells.foldLeft(branches) {
              case (next, linked) =>
                next + (linked -> (next(linked) - value))
            }))
        case None => this
      }

    def advance(steps: Int): Board = steps match {
      case 0 => this
      case _ => advance.advance(steps - 1)
    }

    def solution: Board =
      if (isFinal) this else advance.solution

    def set(cell: Cell, value: Int) =
      Board(
        cells + (cell -> value),
        reduce(branches + (cell -> (branches(cell) & Set(value))) ++
          cell.linkedCells.map(linked =>
            linked -> (branches(linked) - value))))

  }

  object Board {

    val empty = Board(Map.empty, Map.empty.withDefaultValue(all))

    def apply(cells: Map[Cell, Int]): Board =
      cells.foldLeft(Board.empty) {
        case (board, (cell, value)) => board.set(cell, value) }

    def apply(cells: List[Int]): Board = Board(Map.empty ++
      cells.zipWithIndex.collect {
        case (value, index) if value > 0 =>
          Cell(1 + index / (n * n), 1 + index % (n * n)) -> value
      })

  }

}

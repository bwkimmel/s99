package ca.eandb.s99

/**
 * Created with IntelliJ IDEA.
 * User: brad
 * Date: 6/10/12
 * Time: 11:29 PM
 * To change this template use File | Settings | File Templates.
 */

object S99Misc {

  /** P90 */
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

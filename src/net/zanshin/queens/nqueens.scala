package net.zanshin.queens

/**
 * Created with IntelliJ IDEA.
 * User: mhn
 * Date: 11/8/12
 * Time: 9:14 AM
 * To change this template use File | Settings | File Templates.
 */
object nqueens {

  /**
   * Define a recursive algorithm to solve the Queens problem
   * allow for any board size, 4x4, 8x8, etc.
   * @param number Number of rows (and therefore columns) on the board
   * @return Set of solutions, which are expressed as a List of integer column values
   */
  def queens(number: Int): Set[List[Int]] = {

    /**
     * placeQueens puts a numberOfQueens on the board
     * @param numberOfQueens
     * @return Set of solutions, which are expressed as a List of integer column values
     */
    def placeQueens(numberOfQueens: Int): Set[List[Int]] =
      if (numberOfQueens == 0) Set(List())
      else
        for {
          queens <- placeQueens(numberOfQueens - 1)     // recursive call queens with new number
          col <- 0 until n                              // range over all columns
          if isSafe(col, queens)                        // only place queen if it isSafe
        } yield col :: queens                           // result is new col plus all previous queens

    placeQueens(number)                                 // call placeQueens with number from queens
  }

  /**
   * isSafe determines if the new queen can safely go at the current row, column location
   * @param col An integer representing the column
   * @param queens The previous set of queen column placements
   * @return True or false
   */
  def isSafe(col: Int, queens: List[Int]): Boolean = {

    // get the new row value
    val row = queens.length

    // need the list of rows and cols (solution so far is just columns), therefore need to transform queens
    // to add rows. queens is in reverse order so walk through rows in reverse order and zip (merge) with existing
    // queen columns
    val queensWithRow = (row - 1 to 0 by -1) zip queens

    // forall row, column pairs in queensWithRows, make sure the column hasn't already been used AND
    // ensure that the new queen isn't in check over any diagonal. new queen is safe if the absolute
    // difference between the two cols is not equal to the difference between the two rows.
    queensWithRow forall {
      case (r, c) => col != c && math.abs(col - c) != row - r
    }
  }

  /**
   * show displays the resulting solutions in board format. '*' are open squares, and 'Q' are the queens
   * @param queens The solution set
   * @return Outputs a printed visualization of the boards in the solution set
   */
  def show(queens: List[Int]) = {

    val lines =
      for (col <- queens.reverse)
        yield Vector.fill(queens.length)("* ").updated(col, "Q ").mkString "\n" + (lines mkString _"\n")
  }

  /**
   * Run the beast...
   * @param args
   */
  def main(args: Array[String]) {
        queens(4)
  }

}

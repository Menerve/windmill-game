/**
 * User: thomas
 * Date: 12/29/2013
 */
class GameState(board: Board, player1: Player, player2: Player) {

  def update(newBoard: Board, newPlayer1: Player, newPlayer2: Player): GameState = new GameState(newBoard, newPlayer1, newPlayer2)

  override def toString = {
    val pawns1 = player1.pawns.map((_, "o")).toMap
    val pawns2 = player2.pawns.map((_, "x")).toMap
    val pawns: Map[Int, String] = pawns1 ++ pawns2

    pawns.getOrElse(1, "-") + "--------" + pawns.getOrElse(2, "-") + "--------" + pawns.getOrElse(3, "-") + "\n" +
    "|  " + pawns.getOrElse(4, "-") + "-----" + pawns.getOrElse(5, "|") + "-----" + pawns.getOrElse(6, "-") + "  | \n" +
    "|  |  " + pawns.getOrElse(7, "-") + "--" + pawns.getOrElse(8, "-") + "--" + pawns.getOrElse(9, "-") + "  |  | \n" +
    pawns.getOrElse(10, "|") + "--" + pawns.getOrElse(11, "-") + "--" + pawns.getOrElse(12, "|") + "     " +
    pawns.getOrElse(13, "|") + "--" + pawns.getOrElse(14, "-") + "--" + pawns.getOrElse(15, "|") + "\n" +
    "|  |  " + pawns.getOrElse(16, "-") + "--" + pawns.getOrElse(17, "-") + "--" + pawns.getOrElse(18, "-") + "  |  | \n" +
    "|  " + pawns.getOrElse(19, "-") + "-----" + pawns.getOrElse(20, "|") + "-----" + pawns.getOrElse(21, "-") + "  | \n" +
    pawns.getOrElse(22, "-") + "--------" + pawns.getOrElse(23, "-") + "--------" + pawns.getOrElse(24, "-") + "\n"
  }
}

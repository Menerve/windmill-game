/**
 * User: thomas
 * Date: 12/29/2013
 */
class GameState(newWindMillsMade: List[(Int, Int, Int)], board: Board, player1: Player, player2: Player) {

  val windmills = List(
    (1, 2, 3), (4, 5, 6), (7, 8, 9), (10, 11, 12),
    (13, 14, 15), (16, 17, 18), (19, 20, 21), (22, 23, 24),
    (1, 10, 22), (4, 11, 19), (7, 12, 16), (2, 5, 8),
    (17, 20, 23), (9, 13, 18), (6, 14, 21), (3, 15, 24)
  )
  val windMillsMade = newWindMillsMade

  def isNewWindMill(player: Player): List[(Int, Int, Int)] = {
    val pawns = player.pawns
    windmills.filter {
      case (pos1, pos2, pos3) =>
        pawns.contains(pos1) && pawns.contains(pos2) && pawns.contains(pos3) && !windMillsMade.contains((pos1, pos2, pos3))
    }
  }

  def updateWindMillsMade(newWindMill: List[(Int, Int, Int)]) =
    new GameState(windMillsMade ++ newWindMill, board, player1, player2)

  def combinePawns(): Map[Int, String] = {
    val pawns1 = player1.pawns.map((_, "o")).toMap
    val pawns2 = player2.pawns.map((_, "x")).toMap
    pawns1 ++ pawns2
  }

  def update(newBoard: Board, newPlayer1: Player, newPlayer2: Player): GameState =
    new GameState(windMillsMade, newBoard, newPlayer1, newPlayer2)

  override def toString = {

    val pawns: Map[Int, String] = combinePawns()

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

/**
 * User: thomas
 * Date: 11/5/2013
 */
import org.scalatest.FunSuite

class WindmillSuite extends FunSuite {

  val positions: Map[Int, Boolean] = (for {i <- 1 to 24} yield (i, false)).toMap

  val board = new Board(positions)
  val player1 = new Player(List(), List(), 9)
  val player2 = new Player(List(), List(), 9)

  var turnOfP1: Boolean = true
  val gameState = new GameState(board, player1, player2)

  test("Correct initialisation."){
    assert(player1.pawns.isEmpty && player2.pawns.isEmpty, "Pawns of players not empty at start.")
    assert(player1.pawnsRem == 9 && player2.pawnsRem == 9, "Not enough pawns at start.")
    assert(board.availablePositions.size == 24, "All positions must be available.")
    assert(positions.size == 24, "Wrong numbers of positions.")
  }

  test("Player adds pawn."){
    assert(player1.addPawn(3).pawnsRem == player1.pawnsRem - 1, "Pawn has to be removed from remaining when added.")
    assert(player1.addPawn(3).pawns == List(3), "Add new pawn to list of pawns.")
  }

  test("Player removes pawn."){
    val player = player1.addPawn(3)
    assert(player.removePawn(3).pawnsRem == player.pawnsRem, "Remove a pawn doesn't add a remaining pawn.")
    assert(player.removePawn(3).pawns == List(), "Remove pawn from list.")
  }

  test("New windmill."){
    val player = player1.addPawn(1).addPawn(2).addPawn(3)
    assert(!player.isNewWindmill.isEmpty)
  }

  test("Pawns in windmill can't be removed."){
    val player = player1.addPawn(1).addPawn(2).addPawn(3).updateWindmillsMade(List((1, 2, 3)))
    assert(player.removablePawns.isEmpty)
  }
}

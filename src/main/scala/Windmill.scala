/**
 * User: thomas
 * Date: 11/5/2013
 */

object Windmill extends App{

  readLine("Welcome to the Windmill game!\n\n" +
    "The game goes in 2 rounds. The goal is to remove your opponent's pawns.\n" +
    "The game board is represented as follow:\n" +
    "------------------- \n" +
    "|  ------|------  | \n" +
    "|  |  -------  |  | \n" +
    "|-----|     |-----| \n" +
    "|  |  -------  |  | \n" +
    "|  ------|------  | \n" +
    "------------------- \n" +
    "Where all intersections represent a position where a pawn can be placed and indicated by the following values:\n" +
    "1--------2--------3 \n" +
    "|  4-----5-----6  | \n" +
    "|  |  7--8--9  |  | \n" +
    "10-11-12    13-14-15\n" +
    "|  |  16-17-18 |  | \n" +
    "|  19----20----21 | \n" +
    "22-------23-------24\n" +
    "The player 1 is represented by the character o and the player 2 by x.\n" +
    "Press enter to start the game.")

  //TODO: Print phase 1 explications

  val windmills = List(
    (1, 2, 3), (4, 5, 6), (7, 8, 9), (10, 11, 12),
    (13, 14, 15), (16, 17, 18), (19, 20, 21), (22, 23, 24),
    (1, 10, 22), (4, 11, 19), (7, 12, 16), (2, 5, 8),
    (17, 20, 23), (9, 13, 18), (6, 14, 21), (3, 15, 24)
  )

  val positions: Map[Int, Boolean] = (for {i <- 1 to 24} yield (i, false)).toMap

  val player1 = new Player(List())
  val player2 = new Player(List())

  var turnOf: Player = player1

  val board = new Board(positions)
  var gameState = new GameState(board, player1, player2)

  println("Round 1 will now begin. \n" +
    "Players have to placed their 9 pawns on the board on empty positions.\n" +
    "If a player manages to make a windmill (3 pawns aligned in row or column), " +
    "the player can remove a pawn of its opponent. \n" +
    "The round ends when no pawn remains in players' hands.\n")

  def round1(board: Board, player1: Player, player2: Player){
    println(gameState)
    if (turnOf == player1) println("Turn of player 1.") else println("Turn of player 2.")

    if (player1.pawns.size < 9 || player2.pawns.size < 9){
      print("Choose a position for you pawn: ")

      try{
        readInt() match {
          case pos if board.availablePositions contains pos  =>
            if (turnOf == player1){
              if (player2.pawns.size < 9) turnOf = player2 else print("No remaining pawns for player 2\n")
              gameState = gameState.update(board.update(pos), player1.addPawn(pos), player2)
              round1(board.update(pos), player1.addPawn(pos), player2)
            }
            else{
              if (player1.pawns.size < 9) turnOf = player1 else print("No remaining pawns for player 1\n")
              gameState = gameState.update(board.update(pos), player1, player2.addPawn(pos))
              round1(board.update(pos), player1, player2.addPawn(pos))
            }
          case _ =>
            println("Position not available")
            round1(board: Board, player1, player2)
        }
      } catch {
        case ex: NumberFormatException =>
          println("You must choose a number between 1 and 24")
          round1(board: Board, player1, player2)
      }
    }
    else print("end of round 1")
  }

  round1(board, player1, player2)
}

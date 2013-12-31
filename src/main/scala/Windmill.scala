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

  //TODO: Refactor code for testing purpose

  val positions: Map[Int, Boolean] = (for {i <- 1 to 24} yield (i, false)).toMap

  val board = new Board(positions)
  val player1 = new Player(List(), 9)
  val player2 = new Player(List(), 9)

  var turnOfP1: Boolean = true
  var gameState = new GameState(List(), board, player1, player2)

  println("Round 1 will now begin. \n" +
    "Players have to placed their 9 pawns on the board on empty positions.\n" +
    "If a player manages to make a windmill (3 pawns aligned in row or column), " +
    "the player can remove a pawn of its opponent. \n" +
    "The round ends when no pawn remains in players' hands.\n")

  def updateGame(board: Board, player1: Player, player2: Player){

    gameState = gameState.update(board, player1, player2)

    if (checkWindMills(player1, player2)){
      println("You formed a windmill, you can remove a pawn of you opponent! Choose a position: ")
      if (turnOfP1){
        val pos = askForPos(board, player1, player2, player2.pawns)
        updateGame(board.update(pos), player1, player2.removePawn(pos))
      }
      else {
        val pos = askForPos(board, player1, player2, player1.pawns)
        updateGame(board.update(pos), player1.removePawn(pos), player2)
      }
    }
    else{
      turnOfP1 = !turnOfP1
      round1(board, player1, player2)
    }
  }

  def checkWindMills(player1: Player, player2: Player): Boolean = {
    val newWindMill = if (turnOfP1) gameState.isNewWindMill(player1) else gameState.isNewWindMill(player2)
    if (!newWindMill.isEmpty)
      gameState = gameState.updateWindMillsMade(newWindMill)
    !newWindMill.isEmpty
  }

  def askForPos(board: Board, player1: Player, player2: Player, posAvailable: List[Int]): Int = {

    try{
      readInt() match {
        case pos if posAvailable contains pos => pos
        case _ =>
          println("Position not available")
          askForPos(board: Board, player1, player2, posAvailable)
      }
    } catch {
      case ex: NumberFormatException =>
        println("You must choose a number between 1 and 24")
        askForPos(board: Board, player1, player2, posAvailable)
    }
  }

  def round1(board: Board, player1: Player, player2: Player){
    println(gameState)

    if (player1.pawnsRem > 0 || player2.pawnsRem > 0){
      if (turnOfP1) println("Turn of player 1.") else println("Turn of player 2.")
      print("Choose a position for you pawn: ")
      val pos = askForPos(board, player1, player2, board.availablePositions)

      if (turnOfP1) updateGame(board.update(pos), player1.addPawn(pos), player2)
      else updateGame(board.update(pos), player1, player2.addPawn(pos))
    }
    else print("end of round 1")
  }

  round1(board, player1, player2)
}

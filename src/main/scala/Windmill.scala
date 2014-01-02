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
  //TODO: Implement round 2
  //TODO: Implement victory and defeat conditions

  val positions: Map[Int, Boolean] = (for {i <- 1 to 24} yield (i, false)).toMap

  val player1 = new Player(List(), List(), 9)
  val player2 = new Player(List(), List(), 9)

  val gameState = new GameState(positions, player1, player2, turnOf = true)

  println("Round 1 will now begin. \n" +
    "Players have to placed their 9 pawns on the board on empty positions.\n" +
    "If a player manages to make a windmill (3 pawns aligned in row or column), " +
    "the player can remove a pawn of its opponent not in a windmill. \n" +
    "The round ends when no pawn remains in players' hands.\n")

  def askForPos(gameState: GameState, player1: Player, player2: Player, posAvailable: List[Int]): Int = {

    try{
      readInt() match {
        case pos if posAvailable contains pos => pos
        case _ =>
          println("Position not available")
          askForPos(gameState, player1, player2, posAvailable)
      }
    } catch {
      case ex: NumberFormatException =>
        println("You must choose a number between 1 and 24")
        askForPos(gameState, player1, player2, posAvailable)
    }
  }

  trait Round {
    def updateGame(gameState: GameState, player1: Player, player2: Player){

      val newGameState = gameState.update(player1, player2)
      // Check if a windmill has been made by the player
      val newWindMill = if (newGameState.turnOfP1) player1.isNewWindmill else player2.isNewWindmill

      if (!newWindMill.isEmpty && !player1.removablePawns.isEmpty && !player2.removablePawns.isEmpty){
        println("You formed a windmill, you can remove a pawn of you opponent! Choose a position: ")
        // Ask for position among available opponent's pawns
        // and update the board, windmills of the player and remove pawn at position
        if (newGameState.turnOfP1){
          val pos = askForPos(newGameState, player1, player2, player2.removablePawns)
          updateGame(newGameState.updatePositions(pos), player1.updateWindmillsMade(newWindMill), player2.removePawn(pos))
        }
        else {
          val pos = askForPos(newGameState, player1, player2, player1.removablePawns)
          updateGame(newGameState.updatePositions(pos), player1.removePawn(pos), player2.updateWindmillsMade(newWindMill))
        }
      }
      else{
        // Change turn
        run(newGameState.update(player1, player2, !newGameState.turnOfP1), player1, player2)
      }
    }

    def run(gameState: GameState, player1: Player, player2: Player): Unit
  }

  object Round1 extends Round{
    def run(gameState: GameState, player1: Player, player2: Player): Unit = {
      println(gameState)

      if (player1.pawnsRem > 0 || player2.pawnsRem > 0){
        if (gameState.turnOfP1) println("Turn of player 1.") else println("Turn of player 2.")
        print("Choose a position for you pawn: ")
        val pos = askForPos(gameState, player1, player2, gameState.availablePositions)

        // Add pawn for the right player
        if (gameState.turnOfP1) updateGame(gameState.updatePositions(pos), player1.addPawn(pos), player2)
        else updateGame(gameState.updatePositions(pos), player1, player2.addPawn(pos))
      }
      else Round2.run(gameState, player1, player2)
    }
  }

  object Round2 extends Round {
    def run(gameState: GameState, player1: Player, player2: Player){
      if (gameState.turnOfP1){
        println("Turn of player 1.")
        print("Choose a pawn to move: ")
        val pawn = askForPos(gameState, player1, player2, player1.pawns)
        print("Choose a position for this pawn: ")
        val pos = askForPos(gameState, player1, player2, gameState.availablePositions)
        updateGame(gameState.updatePositions(pos), player1.move(pawn, pos), player2)
      }
      else {
        println("Turn of player 2.")
        print("Choose a pawn to move: ")
        val pawn = askForPos(gameState, player1, player2, player2.pawns)
        print("Choose a position for this pawn: ")
        val pos = askForPos(gameState, player1, player2, gameState.availablePositions)
        updateGame(gameState.updatePositions(pos), player1, player2.move(pawn, pos))
      }
    }
  }

  Round1.run(gameState, player1, player2)
}

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

  val player1 = Player(List(), List(), 9)
  val player2 = Player(List(), List(), 9)
  val positions: Map[Int, Boolean] = (for {i <- 1 to 24} yield (i, false)).toMap
  val gameState = GameState(positions, player1, player2, turnOf = true)

  object Round1 extends Round{

    println("Round 1 will now begin. \n" +
      "Players have to placed their 9 pawns on the board on empty positions.\n" +
      "If a player manages to make a windmill (3 pawns aligned in row or column), " +
      "the player can remove a pawn of its opponent not in a windmill. \n" +
      "The round ends when no pawn remains in players' hands.")

    def run(gameState: GameState, player1: Player, player2: Player): Unit = {
       println(gameState)

      if (player1.pawnsRem > 0 || player2.pawnsRem > 0){
        if (gameState.turnOfP1){
          print("Turn of player 1.\nChoose a position for you pawn: ")
          val pos = player1.askForPos(gameState, player1, player2, gameState.availablePositions)
          updateGame(gameState.updatePositions(pos), player1.addPawn(pos), player2)
        }
        else {
          print("Turn of player 2.\nChoose a position for you pawn: ")
          val pos = player2.askForPos(gameState, player1, player2, gameState.availablePositions)
          updateGame(gameState.updatePositions(pos), player1, player2.addPawn(pos))
        }
      }
      else Round2.run(gameState, player1, player2)
    }
  }

  object Round2 extends Round {

    println("Round 2 will now begin. \n" +
      "Players can move their pawns (not in diagonal) to create windmills " +
      "and remove pawns of the opponent.")

    var turnsWithoutWindmills = 0
    var windmillsMade1 = 0
    var windmillsMade2 = 0
    var nTurn = 0
    var countDown = 0
    var gameStateSaved = GameState(positions, player1, player2, turnOf = true)
    var positionsRepeated = 0

    def run(gameState: GameState, player1: Player, player2: Player){

      updateGameOverChecks()
      println(gameState)

      if(!isRoundOver(gameState, player1, player2, nTurn, turnsWithoutWindmills, countDown, positionsRepeated)){
        nTurn += 1
        if (gameState.turnOfP1){
          print("Turn of player 1.\nChoose a pawn to move: ")
          val pawn = player1.askForPos(gameState, player1, player2, player1.movablePawns(gameState.availablePositions))
          print("Choose a position for this pawn: ")
          val pos = player2.askForPos(gameState, player1, player2, player1.availablePositions(pawn, gameState.availablePositions))
          updateGame(gameState.updatePositions(pos).updatePositions(pawn), player1.move(pawn, pos), player2)
        }
        else {
          print("Turn of player 2.\nChoose a pawn to move: ")
          val pawn = player1.askForPos(gameState, player1, player2, player2.movablePawns(gameState.availablePositions))
          print("Choose a position for this pawn: ")
          val pos = player2.askForPos(gameState, player1, player2, player2.availablePositions(pawn, gameState.availablePositions))
          updateGame(gameState.updatePositions(pos).updatePositions(pawn), player1, player2.move(pawn, pos))
        }
      }
      else {
        println("Game over!")
        val winner = checkVictory(gameState, player1, player2)
        if(winner != 0)
          println("The winner is player " + winner + "!")
        else println("It's a tie!")
      }
    }

    def isRoundOver(gameState: GameState, player1: Player, player2: Player, nTurn: Int,
                    turnWithoutWindmills: Int, countDown: Int, positionsRepeated: Int): Boolean = {
      (player1.pawns.size < 3 || player2.pawns.size < 3) ||
        (player1.movablePawns(gameState.availablePositions).isEmpty || player2.movablePawns(gameState.availablePositions).isEmpty) ||
        (turnsWithoutWindmills == 50) ||
        (countDown != 0 && nTurn - countDown == 10) ||
        (positionsRepeated == 3)
    }

    def checkVictory(gS: GameState, player1: Player, player2: Player): Int = {
      if (player1.pawns.size < 3) 2
      else if (player2.pawns.size < 3) 1
      else if (player1.movablePawns(gS.availablePositions).isEmpty) 2
      else if (player2.movablePawns(gS.availablePositions).isEmpty) 1
      else 0
    }

    def updateGameOverChecks(){
      if (nTurn == 0){
        windmillsMade1 = player1.windmillsMade.size
        windmillsMade2 = player2.windmillsMade.size
        gameStateSaved = gameState
      }

      if (player1.windmillsMade.size == windmillsMade1 + 1)
        turnsWithoutWindmills = 0
      else
        turnsWithoutWindmills += 1

      if (player1.pawns.size == 3 && player2.pawns.size == 3 && countDown == 0)
        countDown = nTurn

      if (gameState.availablePositions.size == gameStateSaved.availablePositions.size) {
        if (gameState.positions == gameStateSaved.positions)
          positionsRepeated += 1
      } else {
        gameStateSaved = gameState
        positionsRepeated = 0
      }
    }
  }

  Round1.run(gameState, player1, player2)
}

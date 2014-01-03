/**
 * User: thomas
 * Date: 1/2/2014
 */
trait Round {
  def updateGame(gameState: GameState, player1: Player, player2: Player){

    val newGameState = gameState.update(player1, player2)
    // Check if a windmill has been made by the player
    val newWindMill = if (newGameState.turnOfP1) player1.isNewWindmill else player2.isNewWindmill

    // Check if player can remove a pawn of its opponent
    if (!newWindMill.isEmpty && !player1.removablePawns.isEmpty && !player2.removablePawns.isEmpty){
      println("You formed a windmill, you can remove a pawn of you opponent! Choose a position: ")
      // Ask for position among available opponent's pawns
      // and update the board, windmills of the player and remove pawn at position
      if (newGameState.turnOfP1){
        val pos = player1.askForPos(player2.removablePawns)
        updateGame(newGameState.updatePositions(pos), player1.addWindmill(newWindMill), player2.removePawn(pos))
      }
      else {
        val pos = player2.askForPos(player1.removablePawns)
        updateGame(newGameState.updatePositions(pos), player1.removePawn(pos), player2.addWindmill(newWindMill))
      }
    }
    else{
      // Change turn
      run(newGameState.update(player1, player2, !newGameState.turnOfP1), player1, player2)
    }
  }

  def run(gameState: GameState, player1: Player, player2: Player): Unit
}

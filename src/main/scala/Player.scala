/**
 * User: thomas
 * Date: 12/29/2013
 */
case class Player(windmillsList: List[(Int, Int, Int)], pawnsList: List[Int], limit: Int) {
  val pawns = pawnsList
  val pawnsRem = limit
  val windmillsMade: List[(Int, Int, Int)] = windmillsList

  val windmills = List(
    (1, 2, 3), (4, 5, 6), (7, 8, 9), (10, 11, 12),
    (13, 14, 15), (16, 17, 18), (19, 20, 21), (22, 23, 24),
    (1, 10, 22), (4, 11, 19), (7, 12, 16), (2, 5, 8),
    (17, 20, 23), (9, 13, 18), (6, 14, 21), (3, 15, 24)
  )

  def isNewWindmill: List[(Int, Int, Int)] = {
    windmills.filter {
      case (pos1, pos2, pos3) =>
        pawns.contains(pos1) && pawns.contains(pos2) && pawns.contains(pos3) && !windmillsMade.contains((pos1, pos2, pos3))
    }
  }

  def askForPos(posAvailable: List[Int]): Int = {
    try{
      readInt() match {
        case pos if posAvailable contains pos => pos
        case _ =>
          println("Position not available")
          askForPos(posAvailable)
      }
    } catch {
      case ex: NumberFormatException =>
        println("You must choose a number between 1 and 24")
        askForPos(posAvailable)
    }
  }

  def updateWindmillsMade(newWindmill: List[(Int, Int, Int)]) = Player(windmillsMade ++ newWindmill, pawns, pawnsRem)

  def addPawn(pos: Int): Player = Player(windmillsMade, pawns.+:(pos), pawnsRem - 1)
  def removePawn(pos: Int): Player = Player(windmillsMade, pawns diff List(pos), pawnsRem)
  // Remove windmills from pawns
  def removablePawns: List[Int] =
    pawns diff windmillsMade.unzip3._1 diff windmillsMade.unzip3._2 diff windmillsMade.unzip3._3

  def move(pawn: Int, pos: Int): Player = {
    val newWindmillsMade = windmillsMade.filter{ case (a, b, c) => !(a == pawn || b == pawn || c == pawn) }
    Player(newWindmillsMade, pawns.filter(_ != pawn).+:(pos), pawnsRem)
  }

  def movablePawns(availablePositions: List[Int]): List[Int] = {
    if (pawns.size > 3)
      for {
        pos <- availablePositions
        pawn <- pawns
        if nextTo(pos, pawn)
      } yield pawn
    else pawns
  }

  def nextTo(pos: Int, pawn: Int): Boolean = {
    for (windmill <- windmills)
      if (windmill._1 == pawn && windmill._2 == pos ||
        windmill._1 == pos && windmill._2 == pawn ||
        windmill._2 == pawn && windmill._3 == pos ||
        windmill._2 == pos && windmill._3 == pawn)
        return true
    false
  }

  def availablePositions(pawn: Int, positions: List[Int]): List[Int] =
    if (pawns.size > 3)
      for {
        pos <- positions
        if nextTo(pos, pawn)
      } yield pos
    else positions
}

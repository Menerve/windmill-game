/**
 * User: thomas
 * Date: 12/29/2013
 */
class Player(pawnsList: List[Int]) {
  val pawns = pawnsList

  def addPawn(pos: Int): Player = new Player(pawns.+:(pos))
}

/**
 * User: thomas
 * Date: 12/29/2013
 */
class Player(pawnsList: List[Int], limit: Int) {
  val pawns = pawnsList
  val pawnsRem = limit

  def addPawn(pos: Int): Player = new Player(pawns.+:(pos), pawnsRem - 1)
  def removePawn(pos: Int): Player = new Player(pawns diff List(pos), pawnsRem)
}

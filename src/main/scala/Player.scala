/**
 * User: thomas
 * Date: 12/29/2013
 */
class Player(nPawns: Int) {
  val pawns = nPawns

  def removePawn(pawnsRem: Int): Player = new Player(pawnsRem - 1)
}

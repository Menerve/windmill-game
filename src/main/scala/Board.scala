/**
 * User: thomas
 * Date: 12/29/2013
 */


class Board(positions: Map[Int, Boolean]) {

  //TODO: Merge with GameState?

  def update(position: Int): Board = {
    new Board(positions.updated(position, !positions.get(position).get))
  }

  def availablePositions: List[Int] = positions.filter(!_._2).map{ case (pos, taken) => pos }.toList
}

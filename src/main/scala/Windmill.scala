/**
 * User: thomas
 * Date: 11/5/2013
 */

object Windmill extends App{

  readLine("Welcome to the Windmill game!\n\n" +
    "The game goes in 2 phases. The goal is to remove your opponent's pawns.\n" +
    "The game board is represented as below:\n" +
    "o--------o--------o \n" +
    "|  o-----o-----o  | \n" +
    "|  |  o--o--o  |  | \n" +
    "o--o--o     o--o--o \n" +
    "|  |  o--o--o  |  | \n" +
    "|  o-----o-----o  | \n" +
    "o--------o--------o \n" +
    "Where all o represent a position where a pawn can be placed and indicated by the following values:\n" +
    "1--------2--------3 \n" +
    "|  4-----5-----6  | \n" +
    "|  |  7--8--9  |  | \n" +
    "10-11-12    13-14-15\n" +
    "|  |  16-17-18 |  | \n" +
    "|  19----20----21 | \n" +
    "22-------23-------24\n" +
    "Press enter to start the game.")

  //TODO: Print phase 1 explications
  //TODO: Initialize game


  def guess(max: Int, min: Int) {
    val cur = (max + min) / 2
    readLine("Is the number "+cur+"? (y/n) ") match {
      case "y" => println("I thought so")
      case "n" => {
        def smallerGreater() {
          readLine("Is it smaller or greater? (s/g) ") match {
            case "s" => guess(cur - 1, min)
            case "g" => guess(max, cur + 1)
            case _   => smallerGreater()
          }
        }
        smallerGreater()
      }
      case _   => {
        println("Huh?")
        guess(max, min)
      }
    }
  }
}

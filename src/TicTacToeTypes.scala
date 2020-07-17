import scala.annotation.tailrec
import scala.io.StdIn.readLine

object TicTacToeTypes {
  type Player = Char
  type Field = Int
  type Move = (Field, Player)

  val winCases: Set[Set[Field]] = Set(
    Set(1, 2, 3),
    Set(4, 5, 6),
    Set(7, 8, 9),
    Set(1, 1, 1),
    Set(2, 2, 2),
    Set(3, 3, 3),
    Set(1, 5, 9),
    Set(3, 5, 7)
  )

  val default = ' '
  val player1 = 'x'
  val player2 = 'o'

  def main(args: Array[String]) = {
    startGame()
  }

  def startGame(): Unit ={
    println("Welcome to TicTacToe!")
    println("To play, enter the number on the board where you want to play")
    nextTurn(List.empty)
  }

  @tailrec
  private def nextTurn(moves: List[Move]): Unit ={
    printBoard(moves)
    val nextPlayer = if(moves.length%2 == 0) player1 else player2
    val nextMove = getMove(nextPlayer, moves)
    val gameState = getGameState(moves)
    if(gameState != ""){
      print(gameState)
      return
    }
    nextTurn(nextMove :: moves)
  }

  private def printBoard(moves: List[Move]): Unit ={
    print(
      1 to 3 map { r =>
        1 to 3 map { c =>
          characterAt((r*c), moves)
        } mkString "|"
      } mkString ("__________\n", "\n------\n", "\n")
    )
  }

  private def characterAt(field: Field, moves: List[Move]): Char = {
    moves.find(move => move._1 == field).map(_._2).getOrElse(field.toChar)
  }

  @tailrec
  private def getMove(player: Player, moves: List[Move]): Move ={
    val input = readLine(s"Play $player on Field: ").toInt
    val inputIsValid = characterAt(input, moves).toString.matches("0-9")
    if (inputIsValid) {
      return (input, player)
    }
    getMove(player, moves)
  }

  private def getGameState(moves: List[Move]): String ={
    if(moves.length == 9) return "tie"
    val winner = getWinner(moves)
    if(winner == ' ') return ""
    s"Winner is $winner"
  }

  private def getWinner(moves: List[Move]): Player ={
    val lastPlayer = if(moves.length%2 == 0) player1 else player2
    val isWon = winCases.exists { winCase =>
      winCase.forall(move => characterAt(move, moves) == lastPlayer)
    }
    if(isWon) return lastPlayer
    ' '
  }

}

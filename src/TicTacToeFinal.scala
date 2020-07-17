import scala.annotation.tailrec
import scala.io.StdIn.readLine

object TicTacToeFinal {
  val startBoard: Array[Char] = Array('1', '2', '3', '4', '5', '6', '7', '8', '9')
  val patterns: Set[Set[Int]] = Set(
    Set(0, 1, 2),
    Set(3, 4, 5),
    Set(6, 7, 8),
    Set(0, 3, 6),
    Set(1, 4, 7),
    Set(2, 5, 8),
    Set(0, 4, 8),
    Set(2, 4, 6)
  )

  def main(args: Array[String]): Unit = {
    startGame()
  }

  def startGame(): Unit ={
    println("Welcome to TicTacToe!")
    println("To play, enter the number on the board where you want to play")
    printBoard(startBoard)
    nextTurn(startBoard)
  }

  @tailrec
  private def nextTurn(board: Array[Char]): Unit = {
    val nextMove = readMove(board)
    board(nextMove) = nextPlayer(board)
    printBoard(board)
    if (!isWon(board)) {
      nextTurn(board)
    }
  }

  @tailrec
  private def readMove(board: Array[Char]): Int ={
    try {
      val input = readLine("Input next Turn: ").toInt-1
      if(input<0 || input>8 || !board(input).toString.matches("[1-9]")) {
        throw new Exception
      }
      input
    } catch {
      case _: Exception => readMove(board)
    }
  }

  private def nextPlayer(board: Array[Char]): Char = {
    val remainingTurns = board.count(_.toString.matches("[1-9]"))
    if(remainingTurns%2 == 0) 'x' else 'o'
  }

  private def printBoard(board: Array[Char]): Unit = {
    print(
      0 to 2 map { r =>
        0 to 2 map { c =>
          board(c + r*3)
        } mkString "|"
      } mkString ("__________\n", "\n------\n", "\n")
    )
    println("Next Player is " + nextPlayer(board))
  }

  private def isWon(board: Array[Char]): Boolean = {
    patterns.foreach(pattern=>{
      if(pattern.forall(board(_) == board(pattern.head))) {
        print("Winner is " + board(pattern.head))
        return true
      }
    })
    false
  }
}
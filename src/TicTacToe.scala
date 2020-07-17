import scala.io.StdIn.readLine
import scala.annotation.tailrec

object TicTacToe {

  type Player = Char
  type Coord = (Int, Int)
  type Move = (Coord, Player)

  val winCases: Set[Set[Coord]] = Set(
    Set((1, 1), (1, 2), (1, 3)),
    Set((2, 1), (2, 2), (2, 3)),
    Set((3, 1), (3, 2), (3, 3)),
    Set((1, 1), (2, 1), (3, 1)),
    Set((1, 2), (2, 2), (3, 2)),
    Set((1, 3), (2, 3), (3, 3)),
    Set((1, 1), (2, 2), (3, 3)),
    Set((1, 3), (2, 2), (3, 1))
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
    printBoard(List.empty)

    nextTurn()
  }

  def nextTurn() = {
    @tailrec
    def playRound(moves: List[Move], curr: Player): Option[Player] = {
      println(s"Player $curr's move")
      val move = (nextMove(moves), curr)
      val newMoves = move :: moves
      printBoard(newMoves)
      findWinner(newMoves) match {
        case Right(possWinner) => possWinner
        case Left(_)           => playRound(newMoves, nextPlayer(curr))
      }
    }

    val winner = playRound(List.empty, player1)
    winner match {
      case Some(player) => println(s"Player $player won!")
      case None         => println("Tie")
    }
  }

  private def findWinner(moves: List[Move]): Either[Unit, Option[Player]] =
    if (isWinner(player1, moves)) Right(Some(player1))
    else if (isWinner(player2, moves)) Right(Some(player2))
    else if (isTie(moves)) Right(None)
    else Left(())

  private def isWinner(player: Player, moves: List[Move]): Boolean =
    winCases.exists { winCase =>
      winCase.forall(move => playerAt(move, moves) == player)
    }

  private def isTie(moves: List[Move]): Boolean = moves.size == 9

  private def playerAt(coord: Coord, moves: List[Move]): Player =
    moves.find(move => move._1 == coord).map(_._2).getOrElse(default)

  @tailrec
  private def nextMove(moves: List[Move]): Coord = {
    val coord = (nextRowOrCol("Row"), nextRowOrCol("Column"))

    if (isValid(coord, moves)) {
      coord
    } else {
      println("That move is already taken. Please enter a different move.")
      nextMove(moves)
    }
  }

  private def nextRowOrCol(prompt: String): Int = {
    val input = readLine(s"$prompt: ")

    if (input.matches("[123]")) {
      input.toInt
    } else {
      println("Please enter a number from 1 to 3")
      nextRowOrCol(prompt)
    }
  }

  private def isValid(coord: Coord, moves: List[Move]): Boolean =
    playerAt(coord, moves) == default

  private def nextPlayer(curr: Player): Player =
    if (curr == player1) player2
    else player1

  private def printBoard(moves: List[Move]): Unit =
    print(
      1 to 3 map { r =>
        1 to 3 map { c =>
          playerAt((r, c), moves)
        } mkString "|"
      } mkString ("__________\n", "\n------\n", "\n")
    )
}
import scala.annotation.tailrec

object Solution:
  def run(inputLines: Seq[String]): (String, String) =

    given playGround: PlayGround = PlayGround(parseInput(inputLines))

    val result = walkThrough(playGround.findStart, Down)

    val result1 = s"${result._1}"
    val result2 = s"${result._2}"

    (s"${result1}", s"${result2}")

end Solution

enum Direction:
  case Down, Up, Left, Right

export Direction.*

case class Position(row: Int, col: Int):
  private lazy val nextUp = copy(row = row - 1)
  private lazy val nextDown = copy(row = row + 1)
  private lazy val nextLeft = copy(col = col - 1)
  private lazy val nextRight = copy(col = col + 1)
  def next(currentDirection: Direction)(using playground: PlayGround): Option[(Position, Direction)] =
    def canGoStraight: Option[Position] =
      currentDirection match
        case Down if playground.canReach(nextDown) => Some(nextDown)
        case Up if playground.canReach(nextUp) => Some(nextUp)
        case Left if playground.canReach(nextLeft) => Some(nextLeft)
        case Right if playground.canReach(nextRight) => Some(nextRight)
        case _ => None

    def turn: Option[(Position, Direction)] =
      currentDirection match
        case Down | Up if playground.canReach(nextLeft) => Some((nextLeft, Left))
        case Down | Up if playground.canReach(nextRight) => Some((nextRight, Right))
        case Left | Right if playground.canReach(nextUp) => Some((nextUp, Up))
        case Left | Right if playground.canReach(nextDown) => Some((nextDown, Down))
        case _ => None

    canGoStraight match
      case Some(nextPosition) => Some(nextPosition, currentDirection)
      case None => turn


case class PlayGround(data: Array[Array[Char]]):
  lazy val findStart: Position =
    Position(0, data(0).indexOf('|'))
  def getLetter(position: Position): Option[Char] =
    data(position.row)(position.col) match
      case ' ' | '|' | '-' | '+' => None
      case value => Some(value)

  def canReach(newPosition: Position): Boolean =
    val Position(row, col) = newPosition
    data.isDefinedAt(row) && data(row).isDefinedAt(col) && data(row)(col) != ' '

def parseInput(input: Seq[String]): Array[Array[Char]] = input.map(_.toCharArray).toArray

@tailrec
def walkThrough(currentPosition: Position, currentDirection: Direction, message: StringBuilder = StringBuilder(), counter: Int = 1)(using playGround: PlayGround): (String, Int) =
  currentPosition.next(currentDirection) match
    case None => (message.toString(), counter)
    case Some((newPosition, newDirection)) =>
      playGround.getLetter(newPosition).foreach(message.append)
      walkThrough(newPosition, newDirection, message, counter + 1)

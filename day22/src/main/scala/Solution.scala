object Solution:
  def run(inputLines: Seq[String]): (String, String) =

    val grid = Grid.from(inputLines)
    val middle = (inputLines.head.size / 2)
    val initialPosition = Position(middle, middle)

    given Grid = grid

    val resultPart1 = iterate(initialPosition, Up, 10000)

    val result1 = s"$resultPart1"
    val result2 = s""

    (s"${result1}", s"${result2}")

end Solution

enum Direction:
  case Up, Right, Down, Left
  def turnRight: Direction =
    this match
      case Up => Right
      case Right => Down
      case Down => Left
      case Left => Up

  def turnLeft: Direction =
    this match
      case Up => Left
      case Right => Up
      case Down => Right
      case Left => Down

export Direction.*

def iterate(currentPosition: Position, direction: Direction, steps: Int, infections: Int = 0)(using Grid): Int =
  steps match
    case 0 => infections
    case _ =>
      val next = doBurst(currentPosition, direction)
      iterate(next._1, next._2, steps - 1, infections + next._3)

def doBurst(currentPosition: Position, direction: Direction)(using Grid): (Position, Direction, Int) =
  val (nextDir, infected) = summon[Grid].getPosition(currentPosition.row, currentPosition.col) match
    case true => (direction.turnRight, 0)
    case false => (direction.turnLeft, 1)

  summon[Grid].swapPosition(currentPosition.row, currentPosition.col)
  val nextPosition = currentPosition.next(nextDir)
  (nextPosition, nextDir, infected)

class Grid:
  import scala.collection.mutable.Map
  val positions: Map[Position, Boolean] = Map()
  def setPosition(row: Int, col: Int, value: Boolean) =
    positions.put(Position(row, col), value)
  def swapPosition(row: Int, col: Int) =
    setPosition(row, col, ! getPosition(row, col))
  def getPosition(row: Int, col: Int) =
    positions.getOrElseUpdate(Position(row, col), {
      false
    })

object Grid:
  def from(inputLines: Seq[String]): Grid =
    val newGrid = new Grid()
    inputLines.zipWithIndex.foreach:
      case (line, row) =>
        line.zipWithIndex.foreach:
          case ('#', col) => newGrid.setPosition(row, col, true)
          case _ => ()
    newGrid

case class Position(row: Int, col: Int):
  def next(direction: Direction): Position =
    direction match
      case Up => this.copy(row = row - 1)
      case Down => this.copy(row = row + 1)
      case Left => this.copy(col = col - 1)
      case Right => this.copy(col = col + 1)
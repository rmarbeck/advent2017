import Direction.Left

object Solution:
  def run(inputLines: Seq[String]): (String, String) =

    val middle = inputLines.head.length / 2
    val initialPosition = Position(middle, middle)

    val resultPart1 = {
      given Grid = Grid.from(inputLines)(using OnOffStatus())
      iterate(initialPosition, Up, 10_000)
    }

    val resultPart2 = {
      given Grid = Grid.from(inputLines)(using MultiStatus())
      iterate(initialPosition, Up, 10_000_000)
    }

    val result1 = s"$resultPart1"
    val result2 = s"$resultPart2"

    (s"${result1}", s"${result2}")

end Solution

enum Direction:
  case Up, Right, Down, Left
  private def next(steps: Int) = Direction.fromOrdinal((this.ordinal+steps) % Direction.values.length)
  def opposite: Direction = next(2)
  def turnRight: Direction = next(1)
  def turnLeft: Direction = next(3)

export Direction.*

def iterate(currentPosition: Position, direction: Direction, steps: Int, infections: Int = 0)(using Grid): Int =
  steps match
    case 0 => infections
    case _ =>
      val next = doBurst(currentPosition, direction)
      iterate(next._1, next._2, steps - 1, infections + next._3)

def doBurst(currentPosition: Position, direction: Direction)(using Grid): (Position, Direction, Int) =
  val currentStatus = summon[Grid].getPositionOrInit(currentPosition.row, currentPosition.col)
  val (nextDir, infected) = (currentStatus.nextDirection(direction), currentStatus.getsInfected)
  summon[Grid].setPositionToNextStatus(currentPosition.row, currentPosition.col)
  val nextPosition = currentPosition.next(nextDir)
  (nextPosition, nextDir, infected)

trait Status:
  def infectedVariant: Status
  def next: Status
  def nextDirection(fromDirection: Direction): Direction
  def getsInfected: Int

case class OnOffStatus(infected: Boolean = false) extends Status:
  override def infectedVariant: Status = OnOffStatus(true)
  override def getsInfected: Int =
    infected match
      case true => 0
      case false => 1

  override def next: Status = OnOffStatus(!infected)
  override def nextDirection(fromDirection: Direction): Direction =
    infected match
      case true => fromDirection.turnRight
      case false => fromDirection.turnLeft

enum Part2Status:
  case Clean, Weakened, Infected, Flagged
  def next: Part2Status =
    Part2Status.fromOrdinal((this.ordinal+1) % Part2Status.values.length)

export Part2Status.*

class MultiStatus(status: Part2Status = Clean) extends Status:
  override def infectedVariant: Status = MultiStatus(Infected)
  override def getsInfected: Int =
    status match
      case Weakened => 1
      case _ => 0

  override def next: Status = MultiStatus(status.next)
  override def nextDirection(fromDirection: Direction): Direction =
    status match
      case Clean => fromDirection.turnLeft
      case Weakened => fromDirection
      case Infected => fromDirection.turnRight
      case Flagged => fromDirection.opposite

class Grid(using defaultStatus: Status):
  import scala.collection.mutable.Map
  private val positions: Map[Position, Status] = Map()
  def setPosition(row: Int, col: Int, value: Status): Option[Status] =
    positions.put(Position(row, col), value)
  def setPositionToNextStatus(row: Int, col: Int): Option[Status] =
    setPosition(row, col, getPositionOrInit(row, col).next)
  def getPositionOrInit(row: Int, col: Int): Status =
    positions.getOrElseUpdate(Position(row, col), {
      defaultStatus
    })

object Grid:
  def from(inputLines: Seq[String])(using defaultStatus: Status): Grid =
    val newGrid = new Grid()
    inputLines.zipWithIndex.foreach:
      case (line, row) =>
        line.zipWithIndex.foreach:
          case ('#', col) => newGrid.setPosition(row, col, defaultStatus.infectedVariant)
          case _ => ()
    newGrid

case class Position(row: Int, col: Int):
  def next(direction: Direction): Position =
    direction match
      case Up => this.copy(row = row - 1)
      case Down => this.copy(row = row + 1)
      case Left => this.copy(col = col - 1)
      case Right => this.copy(col = col + 1)
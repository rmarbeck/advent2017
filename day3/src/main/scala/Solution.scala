object Solution:
  def run(inputLines: Seq[String]): (String, String) =

    val square = inputLines.head.toInt

    val resultPart1 = extractSpire(square)

    given DataHolder = DataHolder(Position.InitialPosition, 1)
    given spiralTerator: Iterator[Position] = spiral(Position(0, 1)).iterator

    val resultPart2 = findPart2(square)

    val result1 = s"$resultPart1"
    val result2 = s"$resultPart2"

    (s"${result1}", s"${result2}")

end Solution

def alternatePart1(toFind: Int): Int =
  val finalPosition = (1 until toFind).foldLeft(Position.InitialPosition):
    case (acc, _) => acc.next

  finalPosition.distanceToStart


def findPart2(toFind: Int)(using dataHolder: DataHolder, spiralTerator: Iterator[Position]): Int =
  val found = dataHolder.guess(spiralTerator.next())
  found >= toFind match
    case true => found
    case false => findPart2(toFind)

class DataHolder(initialPosition: Position, initialValue: Int):
  import scala.collection.mutable.Map
  private val data: Map[Position, Int] = Map(initialPosition -> initialValue)
  def guess(position: Position): Int =
    val guessed = position.around.map(get).sum
    data(position) = guessed
    guessed
  private def get(position: Position): Int = data.getOrElse(position, 0)

  override def toString: String =
    val rows = data.keys.map(_.row).toArray.sorted
    val (minRow, maxRow) = (rows.head, rows.last)
    val cols = data.keys.map(_.col).toArray.sorted
    val (minCol, maxCol) = (cols.head, cols.last)
    val asInts =
      for
        row <- minRow to maxRow
        col <- minCol to maxCol
      yield
        get(Position(row, col))

    asInts.grouped(maxCol - minCol + 1).map(_.mkString("\t")).mkString("\n")

def spiral(currentPosition: Position): LazyList[Position] =
  currentPosition #:: spiral(currentPosition.next)

case class Position(row: Int, col: Int):
  lazy val around: List[Position] = {
    for
      curRow <- (row - 1 to row + 1)
      curCol <- (col - 1 to col + 1)
      if curCol != col || curRow != row
    yield
      Position(curRow, curCol)
  }.toList

  private def distanceTo(other: Position): Int =
    (row - other.row).abs + (col - other.col).abs

  lazy val distanceToStart: Int = distanceTo(Position.InitialPosition)

  lazy val next: Position =
    (row, col) match
      case (r, c) if r >= 0 && c == r => this.copy(col = c + 1)
      case (r, c) if r < 0 && c == r  => this.copy(row = r + 1)
      case (r, c) if r <= 0 && c.abs <= -r => this.copy(col = c - 1)
      case (r, c) if c < 0 && r == -c => this.copy(col = c + 1)
      case (r, c) if c <= 0 && r.abs <= -c => this.copy(row = r + 1)
      case (r, c) if c >= 0 && r.abs < c => this.copy(row = r - 1)
      case (_ ,c) => this.copy(col = c + 1)

object Position:
  val InitialPosition = Position(0, 0)


def extractSpire(data: Int): Int =
  type Spire = (Int, Int, Int)
  type SpireCardinals = (Int, Int, Int, Int)
  
  def findSpire(current: Spire = (1, 1, 1)): Spire =
    val (rank, _, max) = current
    data.compareTo(max) match
      case -1 => current
      case _ =>
        findSpire {
          (rank + 1, max + 1, max + 8 * rank)
        }

  def cardinals(from: Spire) : SpireCardinals =
    val (rank, min, max) = from
    val nbElements = max - min + 1
    val east = (max - nbElements) + rank - 1
    val west = east + nbElements/2
    val north = east + nbElements/4
    val south = east + 3*nbElements/4
    (east, north, west, south)

  def distance(from: SpireCardinals): Int =
    from.toList.map(data - _).map(_.abs).min

  val spireValues = findSpire()

  spireValues._1 + distance(cardinals(spireValues)) - 1



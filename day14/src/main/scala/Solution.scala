import scala.annotation.tailrec
import scala.collection.parallel.*
import collection.parallel.CollectionConverters.IterableIsParallelizable

val inputAppendedList: List[Int] = List(17, 31, 73, 47, 23)
val rounds: Int = 64

object Solution:
  def run(inputLines: Seq[String]): (String, String) =

    val asArrayOfBoolean = (0 to 127).map(current => s"${inputLines.head}-$current").par.toArray.map:
      key => calcHash(key.map(_.toInt).toList ::: inputAppendedList, (0 to 255).iterator).toArray.collect:
        case '0' => false
        case '1' => true

    val resultPart1 = asArrayOfBoolean.flatten.count(_ == true)

    val resultPart2 = countGroups2(asArrayOfBoolean, Nil, 0)

    val result1 = s"$resultPart1"
    val result2 = s"$resultPart2"

    (s"${result1}", s"${result2}")

end Solution

@tailrec
def doHash(allLengthList: List[Int], remainingLengthList: List[Int], state: State, remainingRounds: Int): State =
  remainingRounds match
    case 0 => state
    case round =>
      remainingLengthList match
        case Nil => doHash(allLengthList, allLengthList, state, remainingRounds - 1)
        case head :: tail =>
          doHash(allLengthList, tail, state.nextBy(head), remainingRounds)

def calcHash(lengthList: List[Int], data: Iterator[Int]): String =
  doHash(lengthList, lengthList, State.initial(data.toArray), rounds).binaryHash

case class Position(row: Int, col: Int):
  private lazy val up = copy(row = row - 1)
  private lazy val down = copy(row = row + 1)
  private lazy val right = copy(col = col + 1)
  private lazy val left = copy(col = col - 1)
  lazy val around: List[Position] = List(up, down, right, left)

@tailrec
def countGroups(data: Array[Array[Boolean]], currentGroup: List[Position], groups: Int): Int =
  def isUsed(position: Position): Boolean =
    val Position(row, col) = position
    data.isDefinedAt(row) && data(row).isDefinedAt(col) && data(row)(col)
  def isEmpty: Boolean = data.forall(_.forall(_ == false))
  def getContiguousUsed: Option[Position] =
    currentGroup.flatMap:
      position => position.around.find:
        case position: Position => isUsed(position)
    .headOption

  def findUsed: Option[Position] =
    data.zipWithIndex.flatMap:
      case (line, row) =>
        line.zipWithIndex.find(_._1 == true).map((_, col) => Position(row, col))
    .headOption

  isEmpty match
    case true => groups
    case false =>
      getContiguousUsed match
        case Some(position) =>
          data(position.row)(position.col) = false
          countGroups(data, position +: currentGroup, groups)
        case None =>
          findUsed match
            case Some(position) =>
              data(position.row)(position.col) = false
              countGroups(data, List(position), groups + 1)
            case None => groups

@tailrec
def countGroups2(data: Array[Array[Boolean]], currentGroup: List[Position], groups: Int): Int =
  def isUsed(position: Position): Boolean =
    val Position(row, col) = position
    data.isDefinedAt(row) && data(row).isDefinedAt(col) && data(row)(col)
  def isEmpty: Boolean = data.forall(_.forall(_ == false))
  def getContiguousUsed: List[Position] =
    currentGroup match
      case Nil => Nil
      case list => list.head +: list.head.around.filter(isUsed)

  def findUsed: Option[Position] =
    data.zipWithIndex.flatMap:
      case (line, row) =>
        line.zipWithIndex.find(_._1 == true).map((_, col) => Position(row, col))
    .headOption

  isEmpty match
    case true => groups
    case false =>
      getContiguousUsed match
        case Nil =>
          findUsed match
            case Some(position) =>
              data(position.row)(position.col) = false
              countGroups2(data, List(position), groups + 1)
            case None => groups
        case list =>
          val Position(row, col) = list.head
          data(row)(col) = false
          countGroups2(data, list.tail ++: currentGroup.tail, groups)



case class State(data: Array[Int], currentPosition: Int, skipSize: Int):
  lazy val binaryHash: String =
    data.grouped(16).map:
      case list =>
        list.foldLeft(0)(_ ^ _).toBinaryString.pad(8)
    .mkString

  def nextBy(length: Int): State =
    def rearrangeData(): Unit =
      val indexes = (0 until length).map(pos => (pos + currentPosition) % data.size)
      indexes.zip(indexes.reverse.map(data)).foreach:
        case (dest, previous) => data(dest) = previous

    rearrangeData()
    val newCurrent = (currentPosition + length + skipSize) % data.size
    copy(currentPosition = newCurrent, skipSize = skipSize + 1)

object State:
  def initial(data: Array[Int]): State = State(data, 0, 0)

extension (str: String)
  def pad(length: Int): String =
    str match
      case notEnoughChars if notEnoughChars.length < length => s"${"0"*(length-notEnoughChars.length)}$notEnoughChars"
      case enoughChars => enoughChars
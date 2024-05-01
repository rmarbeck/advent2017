import scala.annotation.tailrec

val part2AppendedList: List[Int] = List(17, 31, 73, 47, 23)
val part2Rounds: Int = 64

object Solution:
  def run(inputLines: Seq[String]): (String, String) =

    val lengthExtractor = """(\d+)""".r.unanchored
    val (original, lengthListPart1) = lengthExtractor.findAllIn(inputLines.head).toList.map(_.toInt) match
      case list if list.length == 4 => (0 to 4, list)
      case list => (0 to 255, list)

    val resultPart1 = calcHashPart1(lengthListPart1, original.iterator)

    val lengthListPart2 = inputLines.head.map(_.toInt).toList ::: part2AppendedList

    val resultPart2 = calcHashPart2(lengthListPart2, original.iterator)

    val result1 = s"$resultPart1"
    val result2 = s"$resultPart2"

    (s"${result1}", s"${result2}")

end Solution

@tailrec
def hashPart1(lengthList: List[Int], state: State): Int =
  lengthList match
    case Nil => state.part1Result
    case head :: tail => hashPart1(tail, state.nextBy(head))

def calcHashPart1(lengths: List[Int], data: Iterator[Int]): Int = hashPart1(lengths, State.initial(data.toArray))

@tailrec
def hashPart2(allLengthList: List[Int], remainingLengthList: List[Int], state: State, remainingRounds: Int): State =
  remainingRounds match
    case 0 => state
    case round =>
      remainingLengthList match
        case Nil => hashPart2(allLengthList, allLengthList, state, remainingRounds - 1)
        case head :: tail =>
          hashPart2(allLengthList, tail, state.nextBy(head), remainingRounds)

def calcHashPart2(lengthList: List[Int], data: Iterator[Int]): String =
  hashPart2(lengthList, lengthList, State.initial(data.toArray), part2Rounds).sparseHash

case class State(data: Array[Int], currentPosition: Int, skipSize: Int):
  lazy val part1Result = data(0) * data(1)
  lazy val sparseHash: String =
    data.grouped(16).map:
      case list =>
        list.foldLeft(0)(_ ^ _).toHexString.pad
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
  def pad: String =
    str match
      case singleChar if singleChar.length == 1 => s"0$singleChar"
      case twoChars => twoChars
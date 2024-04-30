import scala.annotation.tailrec

val part2AppendedList: List[Int] = List(17, 31, 73, 47, 23)
val part2Rounds: Int = 64

object Solution:
  def run(inputLines: Seq[String]): (String, String) =

    val lengthExtractor = """(\d+)""".r.unanchored
    val (original, lengthListPart1) = lengthExtractor.findAllIn(inputLines.head).toList.map(_.toInt) match
      case list if list.length == 4 =>
        (0 to 4, list)
      case list =>
        (0 to 255, list)

    val resultPart1 = calcHashPart1(lengthListPart1, original.iterator)

    val lengthListPart2 = inputLines.head.map(_.toInt).toList ::: part2AppendedList

    val resultPart2 = calcHashPart2(lengthListPart2, original.iterator)


    val result1 = s"$resultPart1"
    val result2 = s"$resultPart2"

    (s"${result1}", s"${result2}")

end Solution

case class State(data: Array[Int], currentPosition: Int, skipSize: Int):
  lazy val part1Result = data(0) * data(1)
  lazy val sparseHash: String =
    data.grouped(16).map:
      case list =>
        list.foldLeft(0)(_ ^ _).toHexString match
          case below16 if below16.size == 1 => s"0$below16"
          case value => value
    .mkString
  def nextBy(length: Int): State =
    def rearrangedData: Array[Int] =
      val indexes = (0 until length).map(pos => (pos + currentPosition) % data.size)
      val newData = data.clone()
      indexes.zip(indexes.reverse).foreach:
        case (dest, source) => newData(dest) = data(source)
      newData
    val newCurrent = (currentPosition + length + skipSize) % data.size
    State(rearrangedData, newCurrent, skipSize + 1)

object State:
  def initial(data: Array[Int]): State = State(data, 0, 0)

@tailrec
def hashPart1(lengths: List[Int], state: State): Int =
  lengths match
    case Nil => state.part1Result
    case head :: tail =>
      hashPart1(tail, state.nextBy(head))

def calcHashPart1(lengths: List[Int], data: Iterator[Int]): Int =
  hashPart1(lengths, State.initial(data.toArray))

@tailrec
def hashPart2(allLengths: List[Int], remainingLengths: List[Int], state: State, remainingRounds: Int): State =
  remainingRounds match
    case 0 => state
    case round =>
      remainingLengths match
        case Nil => hashPart2(allLengths, allLengths, state, remainingRounds - 1)
        case head :: tail =>
          hashPart2(allLengths, tail, state.nextBy(head), remainingRounds)

def calcHashPart2(lengths: List[Int], data: Iterator[Int]): String =
  hashPart2(lengths, lengths, State.initial(data.toArray), part2Rounds).sparseHash
object Solution:
  def run(inputLines: Seq[String]): (String, String) =

    val part1Limit = 2017
    val part2Limit = 50_000_000

    val nbOfSteps = inputLines.head.toInt

    val resultPart1 = process(CircularBuffer.init, part1Limit, nbOfSteps).positionAfter(part1Limit)

    val resultPart2 = firstPositions(nbOfSteps, limit = Some(part2Limit)).last

    val result1 = s"$resultPart1"
    val result2 = s"$resultPart2"

    (s"${result1}", s"${result2}")

end Solution

def firstPositions(nbOfSteps: Int, from: Int = 0, currentPosition: Int = 0, currentSize: Int = 1, limit: Option[Int]): LazyList[Int] =
  limit match
    case Some(max) if from > max => LazyList.empty
    case _ =>
      val newPosition = (currentPosition + nbOfSteps) % (currentSize)
      newPosition match
        case 0 => currentSize #:: firstPositions(nbOfSteps, from + 1, 1, currentSize + 1, limit)
        case value => firstPositions(nbOfSteps, from + 1, value + 1, currentSize + 1, limit)

def process(buffer: CircularBuffer, remainingSteps: Int, nbOfSteps: Int, current: Int = 1, currentPosition: Int = 0): CircularBuffer =
  remainingSteps match
    case 0 => buffer
    case _ =>
      val (newBuffer, newCurrentPosition) = buffer.insert(currentPosition, nbOfSteps)
      process(newBuffer, remainingSteps - 1, nbOfSteps, current + 1, newCurrentPosition)

case class CircularBuffer(values: Vector[Int]):
  override def toString: String = values.mkString(",")
  def positionAfter(value: Int): Int =
    values.indexOf(value) match
      case -1 => throw Exception("Not found")
      case index => values((index + 1) % values.size)

  def insert(fromPosition: Int, nbOfSteps: Int): (CircularBuffer, Int) =
    val positionToInsert = (fromPosition + nbOfSteps) % values.size + 1
    val (start, end) = values.splitAt(positionToInsert)
    (CircularBuffer(start ++: (values.size +: end)), positionToInsert)

object CircularBuffer:
  val init: CircularBuffer = CircularBuffer(Vector(0))
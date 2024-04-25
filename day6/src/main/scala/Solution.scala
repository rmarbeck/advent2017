import scala.annotation.tailrec

object Solution:
  def run(inputLines: Seq[String]): (String, String) =

    val slotExtractor = """(\d+)""".r.unanchored

    val memoryBanks = Memory{
      slotExtractor.findAllIn(inputLines.head).toList.zipWithIndex.map:
        case (amount, index) => Slot(index, amount.toInt)
    }

    given Ordering[Slot] = maxThenFirst
    val (resultPart1, resultPart2) = find(memoryBanks)

    val result1 = s"$resultPart1"
    val result2 = s"$resultPart2"

    (s"${result1}", s"${result2}")

end Solution

@tailrec
def find(currentMemory: Memory, previous: Map[String, Int] = Map())(using Ordering[Slot]): (Int, Int) =
  val next = currentMemory.next
  val size = previous.size
  previous.contains(next.toString) match
    case true =>
      val loopSize = size - previous(next.toString)
      (size + 1, loopSize)
    case false => find(next, previous + (next.toString -> size))

case class Slot(rank: Int, amount: Int):
  def add(value: Int) = this.copy(amount = amount + value)
  def empty = this.copy(amount = 0)

val maxThenFirst: Ordering[Slot] =
  (x: Slot, y: Slot) =>
    x._2.compare(y._2) match
      case 0 => -x._1.compare(y._1)
      case value => value

case class Memory(slots: List[Slot]):
  override lazy val toString: String = slots.map(_._2).mkString(" ")
  private lazy val size = slots.length
  def next(using Ordering[Slot]) =
    def getEventuallyEmptiedSlot(index: Int, pivot: Int): Slot =
      index == pivot match
        case true => slots(index).empty
        case false => slots(index)

    val pivotIndex = slots.max._1
    val toSpread = slots(pivotIndex)._2
    Memory {
      slots.indices.toList.map:
        index =>
          val position =
            (index - pivotIndex - 1) % size match
              case value if value >= 0 => value
              case value => value + size
          val nbTurns = toSpread / size
          position >= (toSpread % size) match
            case true => getEventuallyEmptiedSlot(index, pivotIndex).add(nbTurns)
            case false => getEventuallyEmptiedSlot(index, pivotIndex).add(nbTurns + 1)
    }
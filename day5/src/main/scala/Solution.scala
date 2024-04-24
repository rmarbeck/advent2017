import scala.annotation.tailrec

object Solution:
  def run(inputLines: Seq[String]): (String, String) =

    val jumpsForPart1, jumpsForPart2 = inputLines.toArray.collect:
      case value => value.toInt

    val result1 = s"${processPart1(jumpsForPart1)}"
    val result2 = s"${processPart2(jumpsForPart2)}"

    (s"${result1}", s"${result2}")

end Solution

type Updater = Int => Int

@tailrec
def process(jumps: Array[Int], next: Int = 0, steps: Int = 0)(using Updater): Int =
  jumps.isDefinedAt(next) match
    case false => steps
    case true =>
      val jumpTo = next + jumps(next)
      jumps(next) = summon[Updater].apply(jumps(next))
      process(jumps, jumpTo, steps + 1)

def processPart1(jumps: Array[Int]): Int =
  given Updater = _ + 1
  process(jumps)


def processPart2(jumps: Array[Int]): Int =
  given Updater = {
    case value if value >= 3 => value - 1
    case value => value + 1
  }
  process(jumps)
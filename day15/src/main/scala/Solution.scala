import scala.annotation.tailrec
import scala.concurrent.{Await, Future}
import concurrent.ExecutionContext.Implicits.global
import concurrent.duration.DurationInt
import scala.util.Success

val remainder = 2147483647

object Solution:

  private val iterationsPart1 = 40_000_000
  private val iterationsPart2 = 5_000_000

  private val generatorAFactor = 16807
  private val generatorBFactor = 48271

  private val generatorAModulo = 4
  private val generatorBModulo = 8

  def run(inputLines: Seq[String]): (String, String) =

    val s"$_ starts with $startA" = inputLines(0) : @unchecked
    val s"$_ starts with $startB" = inputLines(1) : @unchecked

    val aPart1 = generator(startA.toInt, generatorAFactor).iterator
    val bPart1 = generator(startB.toInt, generatorBFactor).iterator

    val aPart2 = generator(startA.toInt, generatorAFactor, generatorAModulo).iterator
    val bPart2 = generator(startB.toInt, generatorBFactor, generatorBModulo).iterator

    val resultPart1Future = Future {
      counter(aPart1, bPart1, iterationsPart1)
    }

    val resultPart2Future = Future {
      counter(aPart2, bPart2, iterationsPart2)
    }

    Await.ready(Future.sequence(Seq(resultPart1Future, resultPart2Future)), 15.seconds)

    val List(resultPart1, resultPart2) = List(resultPart1Future, resultPart2Future).map(_.value).map:
      case Some(Success(value)) => value
      case _ => 0

    val result1 = s"$resultPart1"
    val result2 = s"$resultPart2"

    (s"${result1}", s"${result2}")

end Solution

@tailrec
def counter(a: Iterator[Long], b: Iterator[Long], remainingIterations: Int, count: Int = 0): Int =
  remainingIterations match
    case 0 => count
    case _ =>
      a.next == b.next match
        case true => counter(a, b, remainingIterations - 1, count + 1)
        case false => counter(a, b, remainingIterations - 1, count)

def generator(currentValue: Long, multiplier: Long, modulo: Int = 1): LazyList[Long] =
  val calculated = (currentValue * multiplier) % remainder

  if (modulo == 1 || calculated % modulo == 0)
    (calculated & 0x0000FFFF) #:: generator(calculated, multiplier, modulo)
  else
    generator(calculated, multiplier, modulo)
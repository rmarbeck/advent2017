import scala.annotation.tailrec
export MainInstruction.{JNZNext, NoUpdate, nextInstruction}

object Solution:
  def run(inputLines: Seq[String]): (String, String) =

    val instructions: Array[Instruction] = inputLines.toArray.map:
      case s"set $value1 $value2" => MainInstruction("set", value1, value2, (_, b) => Some(b))
      case s"sub $value1 $value2" => MainInstruction("sub", value1, value2, (a, b) => Some(a - b))
      case s"mul $value1 $value2" => MainInstruction("mul", value1, value2, (a, b) => Some(a * b), nextInstruction, true)
      case s"jnz $value1 $value2" => MainInstruction("jnz", value1, value2, NoUpdate, JNZNext)
      case _ => throw Exception("Not supported")

    val resultPart1 = processPart1(instructions)(using Registers())

    val resultPart2 = processPart2(instructions)

    val result1 = s"$resultPart1"
    val result2 = s"$resultPart2"

    (s"${result1}", s"${result2}")

end Solution

@tailrec
def processPart1(instructions: Array[Instruction], index: Int = 0, nbMult: Int = 0, part2: Boolean = false)(using registers: Registers): Long =
  instructions.isDefinedAt(index) match
    case false => nbMult
    case true =>
      val instruction = instructions(index)
      if (part2 && index > 10)
        -1
      else
        val newMult = instruction.isToBeCounted match
          case true => nbMult + 1
          case false => nbMult + 0
        instruction.apply() match
          case None => processPart1(instructions, index + 1, newMult, part2)
          case Some(value) => processPart1(instructions, index + value, newMult, part2)

def processPart2(instructions: Array[Instruction]) : Int =
  def extractKeyFigures: (Int, Int, Int) =
    given registers: Registers = Registers(1)
    processPart1(instructions, part2 = true)
    val step = instructions(30) match
      case MainInstruction("sub", _, secondValue, _, _, _) => - secondValue.valueAsString.toInt
      case _ => throw Exception("Not found")
    (registers.byName("b").get.toInt, registers.byName("c").get.toInt, step)
  given PrimeTestRange = primes()
  val (start, end, step) = extractKeyFigures
  (start to end by step).count(isNotPrime)

type PrimeTestRange = Seq[Int]

def isNotPrime(value: Int)(using PrimeTestRange): Boolean =
  ! isPrime(value)

def isPrime(value: Int)(using PrimeTestRange): Boolean =
  summon[PrimeTestRange].span(_ <= Math.sqrt(value))._1.forall(value % _ != 0)

case class SmartValue(valueAsString: String):
  override def toString: String = s"$valueAsString"
  private lazy val isDirect = valueAsString.toLongOption.isDefined
  private lazy val direct = valueAsString.toLong
  def getValue(using Registers): Long =
    isDirect match
      case true => direct
      case false => summon[Registers].byName(valueAsString).get

  def setValue(direct: Long)(using Registers): SmartValue =
    isDirect match
      case true => throw Exception("Unable to set smart value as it is direct")
      case false => summon[Registers].byName(valueAsString).set(direct)
    this
  def setValue(smartValue: SmartValue)(using Registers): SmartValue =
    setValue(smartValue.getValue)


extension (str: String)
  def asSmartValue: SmartValue = SmartValue(str)

trait Instruction:
  def isToBeCounted: Boolean
  def next()(using Registers): Option[Int] = None
  def update()(using Registers): Unit
  final def apply()(using Registers): Option[Int] =
    update()
    next()

type Action = (Long, Long) => Option[Long]
type Next = (Long, Long) => Option[Int]

case class MainInstruction(name: String, mainValue: SmartValue, secondaryValue: SmartValue, action: Action, next: Next, isToBeCounted: Boolean) extends Instruction:
  override def toString: String = s"$name $mainValue $secondaryValue"
  override def next()(using Registers): Option[Int] =
    next.apply(mainValue.getValue, secondaryValue.getValue)

  override def update()(using Registers): Unit =
    action.apply(mainValue.getValue, secondaryValue.getValue).foreach(mainValue.setValue)

object MainInstruction:
  def apply(name: String, registerOrValueAsString: String, valueAsString: String, action: Action, next: Next = nextInstruction, isToBeCounted: Boolean = false) =
    new MainInstruction(name, registerOrValueAsString.asSmartValue, valueAsString.asSmartValue, action, next, isToBeCounted)
  lazy val NoUpdate: Action = (_, _) => None
  lazy val JNZNext: Next = (a, b) => if a != 0 then Some(b.toInt) else None
  lazy val nextInstruction: Next = (_, _) => None

class Registers(initialValueForA: Long = 0l):
  override def toString: String = registers.mkString(",")
  import scala.collection.mutable.Map
  private val registers: Map[String, Register] = Map()
  def byName(name: String): Register =
    name match
      case "a" => registers.getOrElseUpdate(name, Register(initialValueForA))
      case _ => registers.getOrElseUpdate(name, Register(0l))

class Register(initialValue: Long):
  override def toString: String = s"$get"
  private var value = initialValue
  def set(newValue: Long): Register =
    value = newValue
    this
  def get: Long = value


/**
 * Caching primes in a LazyList to test if a number is a prime. Using this list allow to test the number only against true primes when calculating modulo.
 */
def primes(current: List[Int] = Nil): LazyList[Int] =
  def next(highest: Int): Int =
    current.find(highest % _ == 0) match
      case Some(_) => next(highest + 1)
      case None => highest

  current match
    case Nil => 2 #:: primes(List(2))
    case alreadyKnown@ (head :: tail) =>
      val nextFound = next(head)
      nextFound #:: primes(nextFound +: current)

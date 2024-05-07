import scala.annotation.tailrec
export MainInstruction.{JGZNext, NoUpdate}

object Solution:
  def run(inputLines: Seq[String]): (String, String) =

    val instructionsPart1 = inputLines.toArray.map:
      case s"snd $value1" => Sound(value1.asSmartValue)
      case s"rcv $value1" => Recover(value1.asSmartValue)
      case s"set $value1 $value2" => MainInstruction(value1, value2, (_, b) => Some(b))
      case s"add $value1 $value2" => MainInstruction(value1, value2, (a, b) => Some(a + b))
      case s"mul $value1 $value2" => MainInstruction(value1, value2, (a, b) => Some(a * b))
      case s"mod $value1 $value2" => MainInstruction(value1, value2, (a, b) => Some(a % b))
      case s"jgz $value1 $value2" => MainInstruction(value1, value2, NoUpdate, JGZNext)

    val resultPart1 = processPart1(instructionsPart1)(using Registers(0), Queue("Part1"))

    val instructionsPart2 = instructionsPart1.map:
      case Recover(value) => Receive(value)
      case Sound(value) => Send(value)
      case inst: Instruction => inst

    val registers0 = Registers(initialValueForP = 0)
    val registers1 = Registers(initialValueForP = 1)
    val queue0 = Queue("0 -> 1")
    val queue1 = Queue("1 -> 0")
    val program0 = Program(0, instructionsPart2)(using registers0, queue0, queue1)
    val program1 = Program(1, instructionsPart2)(using registers1, queue1, queue0)

    val resultPart2 = processPart2(program0, program1, queue0, queue1)

    val result1 = s"$resultPart1"
    val result2 = s"$resultPart2"

    (s"${result1}", s"${result2}")

end Solution

@tailrec
def processPart1(instructions: Array[Instruction], index: Int = 0)(using registers: Registers, soundQueue: Queue): Long =
  (instructions.isDefinedAt(index), instructions(index)) match
    case (false, _) => throw Exception("Not found")
    case (true, inst: Recover) if inst.isActive =>
      inst.apply()
      inst.lastSoundPlayed
    case (true, instruction) =>
      instruction.apply() match
        case None => processPart1(instructions, index + 1)
        case Some(value) => processPart1(instructions, index + value)

def processPart2(program0: Program, program1: Program, queue0: Queue, queue1: Queue) =
  @tailrec
  def tilBlock: Int =
    val program0Result = program0.run
    val program1Result = program1.run
    (queue0.hasElements, queue1.hasElements) match
      case (false, false) => program1Result
      case _ => tilBlock
  tilBlock

class Program(val id: Int, val instructions: Array[Instruction], var index: Int = 0, var counter: Int = 0)(using registers: Registers, inputQueue: Queue, outputQueue: Queue):
  @tailrec
  final def run: Int =
    (instructions.isDefinedAt(index), instructions(index)) match
      case (false, _) => throw Exception("Not found")
      case (true, inst: Send) =>
        inst.apply()(using registers, outputQueue)
        counter = counter + 1
        index = index + 1
        run
      case (true, inst: Receive) if inst.isReady(using inputQueue) == false =>
        counter
      case (true, instruction) =>
        instruction.apply()(using registers, inputQueue) match
          case None =>
            index = index + 1
            run
          case Some(value) =>
            index = index + value
            run

case class SmartValue(valueAsString: String):
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
  def next()(using Registers, Queue): Option[Int] = None
  def update()(using Registers, Queue): Unit
  final def apply()(using Registers, Queue): Option[Int] =
    update()
    next()

type Action = (Long, Long) => Option[Long]
type Next = (Long, Long) => Option[Int]

class Queue(name: String):
  override def toString: String = s"Q[$name] : ${data.size}"
  var data: Vector[Long] = Vector()
  def hasElements: Boolean = data.nonEmpty
  def enqueue(newValue: Long): Unit =
    data = newValue +: data
  def popLastInserted: Option[Long] =
    hasElements match
      case true =>
        val first = data.head
        data = data.tail
        Some(first)
      case false => None

  def popFirstInserted: Option[Long] =
    hasElements match
      case true =>
        val last = data.last
        data = data.init
        Some(last)
      case false => None

case class MainInstruction(mainValue: SmartValue, secondaryValue: SmartValue, action: Action, next: Next) extends Instruction:
  override def next()(using Registers, Queue): Option[Int] =
    next.apply(mainValue.getValue, secondaryValue.getValue)

  override def update()(using Registers, Queue): Unit =
    action.apply(mainValue.getValue, secondaryValue.getValue).foreach(mainValue.setValue)

object MainInstruction:
  def apply(registerOrValueAsString: String, valueAsString: String, action: Action, next: Next = nextInstruction) =
    new MainInstruction(registerOrValueAsString.asSmartValue, valueAsString.asSmartValue, action, next)
  lazy val NoUpdate: Action = (_, _) => None
  lazy val JGZNext: Next = (a, b) => if a > 0 then Some(b.toInt) else None
  private val nextInstruction: Next = (_, _) => None

case class Sound(mainValue: SmartValue) extends Instruction:
  override def update()(using Registers, Queue): Unit =
    summon[Queue].enqueue(mainValue.getValue)


case class Recover(mainValue: SmartValue) extends Instruction:
  private def getValue(using Registers): Long = mainValue.getValue

  def isActive(using Registers): Boolean = getValue match
    case 0 => false
    case _ => true

  def lastSoundPlayed(using Registers): Long = SmartValue("sound").getValue

  override def update()(using Registers, Queue): Unit =
    isActive match
      case false => ()
      case true =>
        summon[Queue].popLastInserted match
          case None => throw Exception("No Sound Ready")
          case Some(newSound) => SmartValue("sound").setValue(newSound)

case class Send(mainValue: SmartValue) extends Instruction:
  private def getValue(using Registers): Long = mainValue.getValue
  override def update()(using Registers, Queue): Unit = summon[Queue].enqueue(getValue)

case class Receive(mainValue: SmartValue) extends Instruction:
  def isReady(using Queue): Boolean = summon[Queue].hasElements

  override def update()(using Registers, Queue): Unit =
    isReady match
      case false => ()
      case true => mainValue.setValue(summon[Queue].popFirstInserted.get)

class Registers(initialValueForP: Long = 0l):
  import scala.collection.mutable.Map
  private val registers: Map[String, Register] = Map()
  def byName(name: String): Register =
    name match
      case "p" => registers.getOrElseUpdate(name, Register(initialValueForP))
      case _ => registers.getOrElseUpdate(name, Register(0l))

class Register(initialValue: Long):
  private var value = initialValue
  def set(newValue: Long): Register =
    value = newValue
    this
  def get: Long = value
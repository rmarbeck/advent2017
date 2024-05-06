import scala.annotation.tailrec

object Solution:
  def run(inputLines: Seq[String]): (String, String) =

    given Registers = Registers()

    val instructions = inputLines.toArray.map:
      case s"snd $value1" => Sound.from(value1)
      case s"rcv $value1" => Receive.from(value1)
      case s"set $value1 $value2" => MainInstruction(value1, value2, (_, b) => Some(b))
      case s"add $value1 $value2" => MainInstruction(value1, value2, (a, b) => Some(a + b))
      case s"mul $value1 $value2" => MainInstruction(value1, value2, (a, b) => Some(a * b))
      case s"mod $value1 $value2" => MainInstruction(value1, value2, (a, b) => Some(a % b))
      case s"jgz $value1 $value2" => MainInstruction(value1, value2, (_, _) => None, (a, b) => if a > 0 then Some(b.toInt) else None)

    val resultPart1 = process(instructions)

    val result1 = s"$resultPart1"
    val result2 = s""

    (s"${result1}", s"${result2}")

end Solution

@tailrec
def process(instructions: Array[Instruction], index: Int = 0)(using registers: Registers): Long =
  (instructions.isDefinedAt(index), instructions(index)) match
    case (false, _) => throw Exception("Not found")
    case (true, inst: (ReceiveReg | ReceiveInt))=> registers.byName("sound").get
    case (true, instruction) =>
      instruction.apply() match
        case None => process(instructions, index + 1)
        case Some(value) => process(instructions, index + value)

extension (str: String)
  def asRegister(using Registers): Register = summon[Registers].byName(str)
  def isRegister: Boolean =
    str.toIntOption match
      case Some(value) => false
      case _ => true

trait Instruction:
  def next()(using Registers): Option[Int] = None
  def update()(using Registers): Unit = ()
  final def apply()(using Registers): Option[Int] =
    update()
    next()

type Action = (Long, Long) => Option[Long]
type Next = (Long, Long) => Option[Int]

case class MainInstruction(registerAsString: String, valueAsString: String, action: Action, next: Next = MainInstruction.nextInstruction) extends Instruction:
  lazy val value: Option[Long] = valueAsString.toLongOption

  override def next()(using Registers): Option[Int] =
    next.apply(registerAsString.asRegister.get, value.getOrElse(valueAsString.asRegister.get))

  override def update()(using Registers): Unit =
    val register = registerAsString.asRegister
    action.apply(register.get, value.getOrElse(valueAsString.asRegister.get)) match
      case None => ()
      case Some(newValue) => register.set(newValue)

object MainInstruction:
  val nextInstruction: Next = (_, _) => None

case class SoundInt(x: Long)(using registers: Registers) extends Instruction:
  override def update()(using Registers): Unit =
    registers.byName("sound").set(x)
case class SoundReg(register: Register)(using registers: Registers) extends Instruction:
  override def update()(using Registers): Unit = SoundInt(register.get).update()

object Sound:
  def from(valueAsStr: String)(using registers: Registers): Instruction =
    valueAsStr.isRegister match
      case true => SoundReg(valueAsStr.asRegister)
      case false => SoundInt(valueAsStr.toInt)

case class ReceiveInt(x: Long)(using registers: Registers) extends Instruction:
  override def update()(using Registers): Unit =
    println(registers.byName("sound"))
case class ReceiveReg(register: Register)(using registers: Registers) extends Instruction:
  override def update()(using Registers): Unit = ReceiveInt(register.get).update()

object Receive:
  def from(valueAsStr: String)(using registers: Registers): Instruction =
    valueAsStr.isRegister match
      case true => ReceiveReg(valueAsStr.asRegister)
      case false => ReceiveInt(valueAsStr.toInt)

/*case class SetInt(register: Register, value: Long) extends Instruction:
  override def update(): Unit =
    register.set(value)
case class SetRegister(register: Register, valueReg: Register) extends Instruction:
  override def update(): Unit = SetInt(register, valueReg.get).update()

object Set:
  def from(register: Register, valueAsStr: String)(using registers: Registers): Instruction =
    valueAsStr.isRegister match
      case true => SetRegister(register, valueAsStr.asRegister)
      case false => SetInt(register, valueAsStr.toInt)

case class MulInt(register: Register, value: Long) extends Instruction:
  override def update(): Unit =
    register.set(register.get * value)
case class MulRegister(register: Register, valueReg: Register) extends Instruction:
  override def update(): Unit = MulInt(register, valueReg.get).update()

object Mul:
  def from(register: Register, valueAsStr: String)(using registers: Registers): Instruction =
    valueAsStr.isRegister match
      case true => MulRegister(register, valueAsStr.asRegister)
      case false => MulInt(register, valueAsStr.toInt)

case class AddInt(register: Register, value: Long) extends Instruction:
  override def update(): Unit =
    register.set(register.get + value)
case class AddRegister(register: Register, valueReg: Register) extends Instruction:
  override def update(): Unit = AddInt(register, valueReg.get).update()

object Add:
  def from(register: Register, valueAsStr: String)(using registers: Registers): Instruction =
    valueAsStr.isRegister match
      case true => AddRegister(register, valueAsStr.asRegister)
      case false => AddInt(register, valueAsStr.toInt)

case class ModInt(register: Register, value: Long) extends Instruction :
  override def update(): Unit =
    register.set(register.get % value)
case class ModRegister(register: Register, valueReg: Register) extends Instruction:
  override def update(): Unit = ModInt(register, valueReg.get).update()

object Mod:
  def from(register: Register, valueAsStr: String)(using registers: Registers): Instruction =
    valueAsStr.isRegister match
      case true => ModRegister(register, valueAsStr.asRegister)
      case false => ModInt(register, valueAsStr.toInt)

case class JumpInt(register: Register, value: Long) extends Instruction:
  override def next: Option[Int] =
    register.get > 0 match
      case true => Some(value.toInt)
      case false => None

case class JumpRegister(register: Register, valueReg: Register) extends Instruction:
  override def next: Option[Int] = JumpInt(register, valueReg.get).next

object Jump:
  def from(register: Register, valueAsStr: String)(using registers: Registers): Instruction =
    valueAsStr.isRegister match
      case true => JumpRegister(register, valueAsStr.asRegister)
      case false => JumpInt(register, valueAsStr.toInt)*/

class Registers:
  import scala.collection.mutable.Map
  private val registers: Map[String, Register] = Map()
  def byName(name: String): Register =
    registers.getOrElseUpdate(name, Register())

  def max: Long = registers.values.map(_.get).max
  def maxEst: Long = registers.values.map(_.getMax).max

class Register:
  private var value = 0l
  private var maxValue = 0l
  def set(newValue: Long): Register =
    value = newValue
    maxValue = math.max(maxValue, value)
    this
  def reset: Register = set(0l)
  def get: Long = value
  def getMax: Long = maxValue
  def copy(newValue: Long): Register = set(newValue)
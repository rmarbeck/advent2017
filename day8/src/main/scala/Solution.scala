object Solution:
  def run(inputLines: Seq[String]): (String, String) =

    given registers: Registers = Registers()

    val instructions = inputLines.map:
      case s"$register $action $value if $condRegister $test $condValue" => Instruction(register.asRegister, action.asAction, value.toInt, condRegister.asRegister, test.asTest, condValue.toInt)

    instructions.foreach(_.apply)

    val resultPart1 = registers.max
    val resultPart2 = registers.maxEst


    val result1 = s"$resultPart1"
    val result2 = s"$resultPart2"

    (s"${result1}", s"${result2}")

end Solution

case class Instruction(register: Register, action: Action, value: Int, condRegister: Register, test: Test, condValue: Int):
  def apply: Unit =
    test.apply(condRegister, condValue) match
      case true => register.set(action.apply(register, value))
      case false => ()

type Test = (Register, Int) => Boolean
type Action = (Register, Int) => Int

extension (str: String)
  def asRegister(using Registers): Register = summon[Registers].byName(str)
  def asAction: Action =
    str match
      case "inc" => (r, v) => r.get + v
      case "dec" => (r, v) => r.get - v
  def asTest: Test =
    str match
      case "==" => (r, v) => r.get == v
      case "!=" => (r, v) => r.get != v
      case ">=" => (r, v) => r.get >= v
      case "<=" => (r, v) => r.get <= v
      case ">" => (r, v) => r.get > v
      case "<" => (r, v) => r.get < v

class Registers:
  import scala.collection.mutable.Map
  private val registers: Map[String, Register] = Map()
  def byName(name: String): Register =
    registers.getOrElseUpdate(name, Register())

  def max: Int = registers.values.map(_.get).max
  def maxEst: Int = registers.values.map(_.getMax).max

class Register:
  private var value = 0
  private var maxValue = 0
  def set(newValue: Int): Register =
    value = newValue
    maxValue = math.max(maxValue, value)
    this
  def reset: Register = set(0)
  def get: Int = value
  def getMax: Int = maxValue
  def copy(newValue: Int): Register = set(newValue)
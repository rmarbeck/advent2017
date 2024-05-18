import scala.annotation.tailrec

object Solution:
  def run(inputLines: Seq[String]): (String, String) =

    val s"Begin in state ${startingState}." = inputLines.head : @unchecked
    val s"Perform a diagnostic checksum after ${steps} steps." = inputLines(1) : @unchecked

    val states = States(inputLines.drop(2).grouped(10).map(State.parse).toList)

    given States = states
    given Tape = OptimizedTape()

    val resultPart1 = runMachine(states.byName(startingState), steps.toInt)

    val result1 = s"$resultPart1"
    val result2 = s""

    (s"${result1}", s"${result2}")

end Solution

@tailrec
def runMachine(currentState: State, steps: Int)(using Tape, States): Int =
  steps match
    case 0 => summon[Tape].checksum
    case _ => runMachine(currentState.act, steps - 1)

case class Action(toWrite: Int, direction: Direction, nextState: String):
  def act(using tape: Tape): String =
    tape.mark(toWrite)
    tape.move(direction)
    nextState

class States(states: List[State]):
  private val asMap: Map[String, State] = states.map(s => s.name -> s).toMap
  def byName(name: String): State = asMap(name)

case class State(name: String, actionIf0: Action, actionIf1: Action):
  def act(using tape: Tape, states: States): State =
    val nextStateAsString =
      tape.getCurrent match
        case 0 => actionIf0.act
        case 1 => actionIf1.act
    states.byName(nextStateAsString)

trait Tape:
  def checksum: Int
  def getCurrent: Int
  def mark(value: Int): Unit
  def move(direction: Direction): Unit

class StdTape extends Tape:
  def checksum: Int = listOfCursorsWith1.size
  private var cursor: Int = Int.MaxValue / 2
  import scala.collection.mutable.BitSet
  private val listOfCursorsWith1: BitSet = BitSet()

  def getCurrent: Int =
    listOfCursorsWith1.contains(cursor) match
      case true => 1
      case false => 0

  def mark(value: Int): Unit =
    value match
      case 0 => listOfCursorsWith1.remove(cursor)
      case 1 => listOfCursorsWith1.add(cursor)
      case _ => throw Exception("Not supported")

  def move(direction: Direction): Unit =
    direction match
      case Left => cursor = cursor - 1
      case Right => cursor = cursor + 1


class OptimizedTape extends Tape:
  def checksum: Int = listOfPositiveCursorsWith1.size + listOfNegativeCursorsWith1.size
  private var cursor: Int = 0
  import scala.collection.mutable.BitSet
  private val listOfPositiveCursorsWith1: BitSet = BitSet()
  private val listOfNegativeCursorsWith1: BitSet = BitSet()

  private def listBasedOnSignOfCursor: BitSet =
    cursor match
      case value if value >= 0 => listOfPositiveCursorsWith1
      case _ => listOfNegativeCursorsWith1

  def getCurrent: Int =
    listBasedOnSignOfCursor.contains(cursor.abs) match
      case true => 1
      case false => 0

  def mark(value: Int): Unit =
    value match
      case 0 => listBasedOnSignOfCursor.remove(cursor.abs)
      case 1 => listBasedOnSignOfCursor.add(cursor.abs)
      case _ => throw Exception("Not supported")

  def move(direction: Direction): Unit =
    direction match
      case Left => cursor = cursor - 1
      case Right => cursor = cursor + 1


object State:
  def parse(raw: Seq[String]): State =
    val s"In state ${name}:" = raw(1) : @unchecked
    val s"    - Write the value ${valueIf0}." = raw(3) : @unchecked
    val s"    - Move one slot to the ${directionIf0}." = raw(4) : @unchecked
    val s"    - Continue with state ${nextStateIf0}." = raw(5) : @unchecked
    val s"    - Write the value ${valueIf1}." = raw(7) : @unchecked
    val s"    - Move one slot to the ${directionIf1}." = raw(8) : @unchecked
    val s"    - Continue with state ${nextStateIf1}." = raw(9) : @unchecked
    State(name, Action(valueIf0.toInt, directionIf0.toDirection, nextStateIf0), Action(valueIf1.toInt, directionIf1.toDirection, nextStateIf1))


enum Direction:
  case Right, Left
export Direction.*

extension (str: String)
  def toDirection: Direction =
    str match
      case "right" => Right
      case "left" => Left
      case _ => throw Exception("Not recognized")
import scala.annotation.tailrec

object Solution:
  def run(inputLines: Seq[String]): (String, String) =

    val (inputPart1, resultPart2) = removeErasedCharsAndCount(inputLines.head)

    val result1 = s"${countGroups(inputPart1)}"
    val result2 = s"$resultPart2"

    (s"${result1}", s"${result2}")

end Solution

def removeErasedCharsAndCount(input: String) : (String, Int) =
  case class State(input: String, inGarbage: Boolean, outsideGarbage: StringBuilder, garbagedCount: Int):
    def skip1Char = this.copy(input = input.tail)
    def skip2Chars = this.copy(input = input.drop(1).tail)
    def openGarbage = this.copy(input = input.tail, inGarbage = true)
    def closeGarbage = this.copy(input = input.tail, inGarbage = false)
    def appendChar(char: Char) = this.copy(input = input.tail, outsideGarbage = outsideGarbage.append(char))
    def count = this.copy(input = input.tail, garbagedCount = garbagedCount + 1)

  object State:
    def init(input: String) = State(input, false, StringBuilder(), 0)

  @tailrec
  def eraseAndCount(state: State): (String, Int) =
    state.input.isEmpty match
      case true => (state.outsideGarbage.toString, state.garbagedCount)
      case false =>
        (state.inGarbage, state.input(0)) match
          case (false, '<') => eraseAndCount(state.openGarbage)
          case (false, ',') => eraseAndCount(state.skip1Char)
          case (false, char) => eraseAndCount(state.appendChar(char))
          case (true, '!') => eraseAndCount(state.skip2Chars)
          case (true, '>') => eraseAndCount(state.closeGarbage)
          case (true, char) => eraseAndCount(state.count)

  eraseAndCount(State.init(input))

@tailrec
def countGroups(input: String, level: Int = 1, counter: Int = 0): Int =
  input.isEmpty match
    case true => counter
    case false =>
      input(0) match
        case '{' => countGroups(input.tail, level + 1, counter + level)
        case '}' => countGroups(input.tail, level - 1, counter)
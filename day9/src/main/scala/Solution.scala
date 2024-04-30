import scala.annotation.tailrec

object Solution:
  def run(inputLines: Seq[String]): (String, String) =

    val (resultPart1, resultPart2) = removeErasedCharsAndCount(inputLines.head)

    val result1 = s"$resultPart1"
    val result2 = s"$resultPart2"

    (s"${result1}", s"${result2}")

end Solution

def removeErasedCharsAndCount(input: String) : (Int, Int) =
  case class State(input: String, inGarbage: Boolean, garbagedCount: Int, groupsCount: Int, deepness: Int):
    lazy val isEmpty: Boolean = input.isEmpty
    lazy val head: Char = input(0)
    lazy val next: State = copy(input = input.tail)
    def skip1Char: State = next
    def skip2Chars: State = next.next
    def openGarbage: State = copy(inGarbage = true).next
    def closeGarbage: State = copy(inGarbage = false).next
    def garbageInc: State = copy(garbagedCount = garbagedCount + 1).next
    def openGroup: State = copy(groupsCount = groupsCount + deepness, deepness = deepness + 1).next
    def closeGroup: State = copy(deepness = deepness - 1).next

  object State:
    def init(input: String): State = State(input, false, 0, 0, 1)

  @tailrec
  def eraseAndCount(state: State): (Int, Int) =
    state.isEmpty match
      case true => (state.groupsCount, state.garbagedCount)
      case false =>
        (state.inGarbage, state.head) match
          case (false, '{') => eraseAndCount(state.openGroup)
          case (false, '}') => eraseAndCount(state.closeGroup)
          case (false, '<') => eraseAndCount(state.openGarbage)
          case (false, ',') => eraseAndCount(state.skip1Char)
          case (true, '!') => eraseAndCount(state.skip2Chars)
          case (true, '>') => eraseAndCount(state.closeGarbage)
          case (true, char) => eraseAndCount(state.garbageInc)
          case _ => throw Exception("Not supported")

  eraseAndCount(State.init(input))
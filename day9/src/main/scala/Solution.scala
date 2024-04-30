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
    type FinalResult = (Int, Int)
    type IntermediateComputation = (Boolean, Char)
    lazy val currentStatus: Either[IntermediateComputation, FinalResult] =
      isFinal match
        case true => Right(result)
        case false => Left(current)
    lazy val isFinal: Boolean = input.isEmpty
    lazy val result: FinalResult = (groupsCount, garbagedCount)
    lazy val current: IntermediateComputation = (inGarbage, input(0))
    lazy val next: State = copy(input = input.tail)
    lazy val skip1Char: State = next
    lazy val skip2Chars: State = next.next
    lazy val openGarbage: State = next.copy(inGarbage = true)
    lazy val closeGarbage: State = next.copy(inGarbage = false)
    lazy val garbageInc: State = next.copy(garbagedCount = garbagedCount + 1)
    lazy val openGroup: State = next.copy(groupsCount = groupsCount + deepness, deepness = deepness + 1)
    lazy val closeGroup: State = next.copy(deepness = deepness - 1)

  object State:
    def init(input: String): State = State(input, false, 0, 0, 1)

  @tailrec
  def eraseAndCount(state: State): (Int, Int) =
    state.currentStatus match
      case Right(result) => result
      case Left(onGoingStatus) =>
        onGoingStatus match
          case (false, '{') => eraseAndCount(state.openGroup)
          case (false, '}') => eraseAndCount(state.closeGroup)
          case (false, '<') => eraseAndCount(state.openGarbage)
          case (false, ',') => eraseAndCount(state.skip1Char)
          case (true, '!') => eraseAndCount(state.skip2Chars)
          case (true, '>') => eraseAndCount(state.closeGarbage)
          case (true, _) => eraseAndCount(state.garbageInc)
          case _ => throw Exception("Not supported")

  eraseAndCount(State.init(input))
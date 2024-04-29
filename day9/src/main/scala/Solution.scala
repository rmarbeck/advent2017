import scala.annotation.tailrec

object Solution:
  def run(inputLines: Seq[String]): (String, String) =

    val (inputPart1, resultPart2) = removeErasedCharsAndCount(inputLines.head)

    val result1 = s"${countGroups(inputPart1)}"
    val result2 = s"$resultPart2"

    (s"${result1}", s"${result2}")

end Solution

def removeErasedCharsAndCount(input: String) : (String, Int) =
  @tailrec
  def eraseAndCount(input: String, inGarbage: Boolean, outsideGarbage: StringBuilder, garbagedCount: Int): (String, Int) =
    input.isEmpty match
      case true => (outsideGarbage.toString, garbagedCount)
      case false =>
        (inGarbage, input(0)) match
          case (false, '<') => eraseAndCount(input.tail, true, outsideGarbage, garbagedCount)
          case (false, ',') => eraseAndCount(input.tail, false, outsideGarbage, garbagedCount)
          case (false, char) => eraseAndCount(input.tail, false, outsideGarbage.append(char), garbagedCount)
          case (true, '!') => eraseAndCount(input.drop(1).tail, true, outsideGarbage, garbagedCount)
          case (true, '>') => eraseAndCount(input.tail, false, outsideGarbage, garbagedCount)
          case (true, char) => eraseAndCount(input.tail, true, outsideGarbage, garbagedCount + 1)

  eraseAndCount(input, false, StringBuilder(), 0)

@tailrec
def countGroups(input: String, level: Int = 1, counter: Int = 0): Int =
  input.isEmpty match
    case true => counter
    case false =>
      input(0) match
        case '{' => countGroups(input.tail, level + 1, counter + level)
        case '}' => countGroups(input.tail, level - 1, counter)
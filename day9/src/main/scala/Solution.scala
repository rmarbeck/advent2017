import scala.annotation.tailrec

object Solution:
  def run(inputLines: Seq[String]): (String, String) =

    val result1 = s"${countGroups(inputLines.head.clean)}"
    val result2 = s"${countInGarbage(inputLines.head)}"

    (s"${result1}", s"${result2}")

end Solution

def removeErasedChars(input: String) : String =
  input.replaceAll("!.", "")

def countInGarbage(input: String) : Int =
  def count(input: String, inGarbage: Boolean, counter: Int): Int =
    input.isEmpty match
      case true => counter
      case false =>
        (inGarbage, input(0)) match
          case (false, '<') => count(input.tail, true, counter)
          case (false, char) => count(input.tail, false, counter)
          case (true, '!') => count(input.drop(1).tail, true, counter)
          case (true, '>') => count(input.tail, false, counter)
          case (true, char) => count(input.tail, true, counter + 1)

  count(input, false, 0)

def removeErasedCharsInGarbageOnly(input: String) : String =
  def erase(input: String, inGarbage: Boolean, current: StringBuilder): String =
    input.isEmpty match
      case true => current.toString()
      case false =>
        (inGarbage, input(0)) match
          case (false, '<') => erase(input.tail, true, current.append('<'))
          case (false, char) => erase(input.tail, false, current.append(char))
          case (true, '!') => erase(input.drop(1).tail, true, current)
          case (true, '>') => erase(input.tail, false, current.append('>'))
          case (_, char) => erase(input.tail, inGarbage, current.append(char))

  erase(input, false, StringBuilder())

def throwGarbageAway(input: String) : String =
  input.replaceAll("<[^>]*>", "")

extension (str: String)
  def clean: String =
    throwGarbageAway(removeErasedChars(str))
  def isEnclosed: Boolean =
    @tailrec
    def parse(current: String, opened: Int): Boolean =
      (current.isEmpty, opened) match
        case (false, 0) => false
        case (true, 0) => true
        case (true, _) => throw Exception(s"Not supported : '$current' ($opened)")
        case _ =>
          current(0) match
            case '{' => parse(current.tail, opened + 1)
            case '}' => parse(current.tail, opened - 1)
            case _ => parse(current.tail, opened)

    str.head match
      case '{' => parse(str.tail, 1)
      case value => false


  def unwrap: String = str.drop(1).dropRight(1)

def countGroups(input: String, level: Int = 1): Int =
  def topLevelGroups2(inside: String): List[String] =
    println(s"start : $inside")
    val result = inside.scanLeft((Set(""): Set[String], 0)):
      case ((acc, 0), ',') => (acc + "", 0)
      case ((acc, deepness), '{') => (acc.init.+(s"${acc.last}{"), deepness + 1)
      case ((acc, deepness), '}') => (acc.init.+(s"${acc.last}}"), deepness - 1)
      case ((acc, deepness), char) => (acc.init + s"${acc.last}${char}", deepness)
    println(s"end : $result")
    result.last._1.toList

  def topLevelGroups(inside: String): List[String] =
    val result = inside.scanLeft((List(""): List[String], 0)):
      case ((acc, 0), ',') => ("" +: acc, 0)
      case ((acc, deepness), '{') => (s"${acc.head}{" +: acc.tail, deepness + 1)
      case ((acc, deepness), '}') => (s"${acc.head}}" +: acc.tail, deepness - 1)
      case ((acc, deepness), char) => (s"${acc.head}$char" +: acc.tail, deepness)
    result.last._1


  input.isEmpty match
    case true => 0
    case false =>
      input.isEnclosed match
        case true => level + countGroups(input.unwrap, level + 1)
        case false => topLevelGroups(input).foldLeft(0)((acc, partial) => acc + countGroups(partial, level))
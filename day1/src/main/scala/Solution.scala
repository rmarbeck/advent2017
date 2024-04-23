object Solution:
  def run(inputLines: Seq[String]): (String, String) =

    val input = inputLines.head.toCharArray

    val result = input.foldLeft((0, None: Option[Char])):
      case ((currentSum, Some(previous)), newChar) if newChar == previous => (currentSum + newChar.asDigit, Some(newChar))
      case ((currentSum, _), newChar) => (currentSum , Some(newChar))

    val resultPart1 = result._2 match
      case Some(value) if input.head == value => result._1 + value.asDigit
      case _ => result._1

    val (start, end) = input.splitAt(input.length / 2)
    val resultPart2 = start.zip(end).collect:
      case (a, b) if a == b => a.asDigit * 2
    .sum

    val result1 = s"$resultPart1"
    val result2 = s"$resultPart2"

    (s"${result1}", s"${result2}")

end Solution

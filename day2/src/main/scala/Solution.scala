object Solution:
  def run(inputLines: Seq[String]): (String, String) =

    val numberExtractor = """(\d+)""".r.unanchored
    val figures: Seq[List[Int]] = inputLines.map(numberExtractor.findAllIn(_).map(_.toInt).toList)

    val resultPart1 = figures.map:
      listOfFigures =>
        val sorted = listOfFigures.sorted.toArray
        sorted.last - sorted.head
    .sum

    val resultPart2 =
      figures.map:
          _.combinations(2).collect:
            case List(a, b) if a % b == 0 => a / b
            case List(a, b) if b % a == 0 => b / a
          .sum
      .sum

    val result1 = s"$resultPart1"
    val result2 = s"$resultPart2"

    (s"${result1}", s"${result2}")

end Solution

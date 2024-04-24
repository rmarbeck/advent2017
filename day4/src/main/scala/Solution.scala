object Solution:
  def run(inputLines: Seq[String]): (String, String) =

    val (part1, part2) = inputLines.map:
      passPhrase =>
        val eachPairs = passPhrase.split(" ").combinations(2).toList.map:
          case Array(a, b) => (a != b, a.sorted.mkString != b.sorted.mkString)

        (eachPairs.exists(_._1 == false), eachPairs.exists(_._2 == false))
    .unzip

    val resultPart1 = part1.count(_ == false)
    val resultPart2 = part2.count(_ == false)

    val result1 = s"$resultPart1"
    val result2 = s"$resultPart2"

    // Or, perhaps simpler to understand
    /*
    val resultPart1b = solve(inputLines, identity)
    val resultPart2b = solve(inputLines, word => word.sorted.mkString)
     */
    
    (s"${result1}", s"${result2}")

end Solution

type Mapper = String => String

def solve(input: Seq[String], mapper: Mapper): Int =
  input.count:
    _.split(" ").combinations(2).forall:
      case Array(a, b) if mapper.apply(a) != mapper.apply(b) => true
      case _ => false
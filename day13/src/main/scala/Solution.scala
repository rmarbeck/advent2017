object Solution:
  def run(inputLines: Seq[String]): (String, String) =

    val layers = inputLines.collect:
      case s"$depth: $size" => Layer(depth.toInt, size.toInt)

    val resultPart1 = layers.map(_.severity).sum
    val resultPart2 = brut(0, layers)

    val result1 = s"$resultPart1"
    val result2 = s"$resultPart2"

    (s"${result1}", s"${result2}")

end Solution

case class Layer(depth: Int, size: Int):
  private lazy val caught: Boolean = caught(0)
  def caught(withDelay: Int): Boolean = (depth + withDelay) % ((size-1) * 2) == 0
  lazy val severity: Int =
    caught match
      case false => 0
      case true => depth * size

def brut(current: Int = 0, layers: Seq[Layer]): Int =
  layers.find(_.caught(current)) match
    case None => current
    case _ => brut(current + 1, layers)



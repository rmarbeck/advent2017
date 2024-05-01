object Solution:
  def run(inputLines: Seq[String]): (String, String) =

    val destination = inputLines.head.split(",").foldLeft((Position.start, 0)):
      case ((pos, maxDistance), direction) =>
        val newPosition = direction match
          case "n" => pos.n
          case "ne" => pos.ne
          case "nw" => pos.nw
          case "s" => pos.s
          case "se" => pos.se
          case "sw" => pos.sw
          case _ => throw Exception("Not supported")
        (newPosition, math.max(maxDistance, newPosition.distanceToStart))

    val resultPart1 = destination._1.distanceToStart
    val resultPart2 = destination._2

    val result1 = s"$resultPart1"
    val result2 = s"$resultPart2"

    (s"${result1}", s"${result2}")

end Solution

case class Position(row: Int, col: Int):
  def n: Position = copy(row = row - 2)
  def ne: Position = copy(row = row - 1, col = col + 1)
  def nw: Position = copy(row = row - 1, col = col - 1)
  def s: Position = copy(row = row + 2)
  def se: Position = copy(row = row + 1, col = col + 1)
  def sw: Position = copy(row = row + 1, col = col - 1)

  def distanceToStart: Int = distanceTo(Position.start)

  private def distanceTo(other: Position): Int =
    ((row - other.row).abs + (col - other.col).abs) / 2

object Position:
  val start: Position = Position(0, 0)
import scala.collection.immutable.ListMap

object Solution:
  def run(inputLines: Seq[String]): (String, String) =

    val xyzExtractor = """(-?\d+)""".r.unanchored

    val particles =
      inputLines.zipWithIndex.map:
        case (line, row) =>
          val Seq(Seq(px, py, pz), Seq(sx, sy, sz), Seq(ax, ay, az)) = xyzExtractor.findAllIn(line).map(_.toInt).grouped(3).toSeq
          Particle(row, XYZ(px, py, pz), XYZ(sx, sy, sz), XYZ(ax, ay, az))

    val resultPart1 = particles.minBy(_.globalAcceleration).id

    val resultPart2 = findNonColliding(particles.toList)

    val result1 = s"$resultPart1"
    val result2 = s"$resultPart2"

    (s"${result1}", s"${result2}")

end Solution

type Position = XYZ
type Speed = XYZ
type Acceleration = XYZ

case class XYZ(x: Int, y: Int, z: Int)

case class Particle(id: Int, position: Position, speed: Speed, accel: Acceleration):
  override def toString: String = s"P[$id]"
  lazy val globalAcceleration: Int =
    math.abs(accel.x) + math.abs(accel.y) + math.abs(accel.z)

  def willCollideAt(other: Particle): List[Int] =
    def willCollideOnCoordinateAt(coordinatesExtractor: XYZ => Int): Option[List[Int]] =
      def extract(extractor: Particle => XYZ): Int =
        coordinatesExtractor.apply(extractor.apply(this)) - coordinatesExtractor.apply(extractor.apply(other))

      // 1 / 2 a t² + (s + 1 / 2 a) t + p <=> a t² + (2 s + a) t + 2 p
      val equation = Equation(extract(_.accel), extract(_.speed) * 2 + extract(_.accel), extract(_.position) * 2)
      equation.isTheSame match
        case true => None
        case false => Some(equation.positiveIntSolutions)

    val byCoordinates = List[XYZ => Int](_.x, _.y, _.z).map(willCollideOnCoordinateAt).flatten
    byCoordinates.tail.foldLeft(byCoordinates.head):
      case (acc, newList) => acc intersect newList

case class Equation(a: Int, b: Int, c: Int):
  override def toString: String = s"$a x² + $b x + $c => $positiveIntSolutions"
  lazy val isTheSame: Boolean = a == 0 && b == 0 && c == 0
  def positiveIntSolutions: List[Int] =
    def asPositiveInt(input: Double): Option[Int] =
      input match
        case value if value > 0 && value.toInt == value => Some(value.toInt)
        case _ => None

    val possibleSolutions = (a, delta) match
      case (0, _) =>
        b match
          case 0 if c == 0 => throw Exception("Not supported : equations are the same")
          case 0 => Nil
          case _ => List(asPositiveInt(-c.toDouble / b.toDouble))
      case (_, value) if value < 0 => Nil
      case (_, 0) => List(asPositiveInt(- b.toDouble / (2 * a.toDouble)))
      case (_, value) => 
        List(
          asPositiveInt(
            (-b.toDouble - Math.sqrt(delta)) / (2 * a.toDouble)
          )
        ,
          asPositiveInt(
            (-b.toDouble + Math.sqrt(delta)) / (2 * a.toDouble)
          )
        )

    possibleSolutions.flatten

  private lazy val delta: Double = (b * b) - (4 * a * c)

def findNonColliding(particles: List[Particle]): Int =
  val roots = particles.combinations(2).collect:
    case List(one, other) => (List(one, other), one willCollideAt other)
  val byRoots = roots.flatMap:
    case (particles, commonRoots) => commonRoots.map(_ -> particles)
  .toList.groupMap(_._1)(_._2)
  val sortedRoots = ListMap[Int, List[List[Particle]]](byRoots.toSeq.sortBy(_._1):_*)
  val collidingParticles = sortedRoots.foldLeft(Set[Particle]()):
    case (acc, (_, particlesList)) =>
      acc ++ (particlesList.filterNot:
        _.exists(currentColliding => acc contains currentColliding)).flatten

  particles.size - collidingParticles.size
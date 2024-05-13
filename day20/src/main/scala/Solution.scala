import scala.annotation.tailrec

object Solution:
  def run(inputLines: Seq[String]): (String, String) =

    val xyzExtractor = """(-?\d+)""".r.unanchored

    val particles =
      inputLines.zipWithIndex.map:
        case (line, row) =>
          val List(List(px, py, pz), List(sx, sy, sz), List(ax, ay, az)) = xyzExtractor.findAllIn(line).toList.map(_.toInt).grouped(3).toList
          Particle(row, XYZ(px, py, pz), XYZ(sx, sy, sz), XYZ(ax, ay, az))

    val resultPart1 = particles.sortBy(_.globalAcceleration).head.id

    val resultPart2 = findNonColliding(particles.toList)

    val result1 = s"$resultPart1"
    val result2 = s"$resultPart2"

    (s"${result1}", s"${result2}")

end Solution

type Position = XYZ
type Speed = XYZ
type Acceleration = XYZ

case class XYZ(x: Int, y: Int, z: Int):
  def +(other: XYZ): XYZ =
    XYZ(x + other.x, y + other.y, z + other.z)

case class MinMax(min: Option[Int], max: Option[Int]):
  def overlap(other: MinMax): Boolean =
    (min, other.min, max, other.max) match
      case (None, None, _, _) => true
      case (_, _, None, None) => true
      case (None, Some(om), Some(m), None) if om >= m => true
      case (Some(om), None, None, Some(m)) if om <= m => true
      case _ => false

def minMax(current: Int, speed: Int, accel: Int): MinMax =
  val (min: Option[Int], max: Option[Int]) = (accel, speed) match
    case (0, 0) => (Some(current), Some(current))
    case (0, s) if s > 0 => (Some(current), None)
    case (0, s) if s < 0 => (None, Some(current))
    case (a, s) if a > 0 && s >= 0 => (Some(current), None)
    case (a, s) if a < 0 && s <= 0 => (None, Some(current))
    case (a, s) if a > 0 =>
      val min: Double = -s / (2 * accel)
      val atMin: Double = 1/2 * accel * min * min + speed * min + current
      (Some(math.min(atMin, current).toInt), None)
    case (a, s) if a < 0 =>
      val max: Double = -s / (2 * accel)
      val atMax: Double = (1/2) * accel * max * max + speed * max + current
      (None, Some(math.max(atMax, current).toInt))
  MinMax(min, max)

case class Particle(id: Int, position: Position, speed: Speed, accel: Acceleration):
  def willCollide(other: Particle): Boolean =
    def willCollideOn(coordinatesExtractor: XYZ => Int): Equation =
      def extractData(extractor: Particle => XYZ): Int =
        coordinatesExtractor.apply(extractor.apply(this)) - coordinatesExtractor.apply(extractor.apply(other))
      Equation(1/2 * extractData(_.accel), extractData(_.speed), extractData(_.position))
    willCollideOn(_.x).isTheSame
    
    ((Equation(1/2 * (accel.x - other.accel.x), (speed.x - other.speed.x), (position.x - other.position.x)).positiveIntSolutions intersect
      Equation(1/2 * (accel.y - other.accel.y), (speed.y - other.speed.y), (position.y - other.position.y)).positiveIntSolutions) intersect
        Equation(1/2 * (accel.z - other.accel.z), (speed.z - other.speed.z), (position.z - other.position.z)).positiveIntSolutions).nonEmpty
  lazy val atT0: ParticleAtInstantT = ParticleAtInstantT(this, 0, position, speed)
  lazy val globalAcceleration: Int =
    math.abs(accel.x) + math.abs(accel.y) + math.abs(accel.z)

case class ParticleAtInstantT(particle: Particle, time: Int, currentPosition: Position, currentSpeed: Speed):
  lazy val minMaxX = minMax(currentPosition.x, currentSpeed.x, particle.accel.x)
  lazy val minMaxY = minMax(currentPosition.y, currentSpeed.y, particle.accel.y)
  lazy val minMaxZ = minMax(currentPosition.z, currentSpeed.z, particle.accel.z)
  lazy val next: ParticleAtInstantT =
    val newSpeed = currentSpeed + particle.accel
    val newPosition = currentPosition + newSpeed
    this.copy(time = time + 1, currentPosition = newPosition, currentSpeed = newSpeed)
  def canCollideWith(other: ParticleAtInstantT): Boolean =
    minMaxX.overlap(other.minMaxX) && minMaxY.overlap(other.minMaxY) && minMaxZ.overlap(other.minMaxZ)
  def isCollidingWith(other: ParticleAtInstantT): Boolean = currentPosition == other.currentPosition


case class Equation(a: Int, b: Int, c: Int):
  lazy val isTheSame: Boolean = a == 0 && b == 0 && c == 0
  def positiveIntSolutions: List[Int] =
    def asPositiveInt(input: Double): Option[Int] =
      val result = input match
        case value if value > 0 => input.toInt == input
        case _ => false
      result match
        case true => Some(input.toInt)
        case false => None

    val possibleSolutions = (a, delta) match
      case (0, _) =>
        b match
          case 0 if c == 0 => (0 to 1000).map(Some(_)).toList
          case 0 => Nil
          case _ => List(asPositiveInt(-c / b))
      case (_, value) if value < 0 => Nil
      case (_, 0) => List(asPositiveInt(- b / (2 * a)))
      case (_, value) => 
        List(
          asPositiveInt(
            (-b - Math.sqrt(delta)) / (2 * a)
          )
        ) ::: List(
          asPositiveInt(
            (-b + Math.sqrt(delta)) / (2 * a)
          )
        )

    possibleSolutions.flatten

  lazy val delta: Double = (b * b) - (4 * a * c)


def findNonColliding(particles: List[Particle]): Int =
  def step(remaining: List[Particle], nonColliding: List[Particle]): Int =
    remaining match
      case Nil => nonColliding.size
      case head :: Nil => nonColliding.size + 1
      case head :: tail =>
        val toExclude = tail.filter(_ willCollide head)
        toExclude match
          case Nil => step(tail, head +: nonColliding)
          case list => step(tail diff list, nonColliding)
  step(particles, Nil)

@tailrec
def findUnlimitedLifeParticles(particles: List[ParticleAtInstantT]): Int =
  println(particles.size)
  val collidingParticles =
    particles.combinations(2).filter:
      case List(one, other) if one.isCollidingWith(other) => true
      case _ => false
    .flatten.toList.distinct
  val remainingParticles = particles diff collidingParticles
  val canOtherCollide =
    remainingParticles.combinations(2).exists:
      case List(one, other) if one.canCollideWith(other) => true
      case _ => false
  canOtherCollide match
    case false => remainingParticles.size
    case true => findUnlimitedLifeParticles(remainingParticles.map(_.next))
object Solution:
  def run(inputLines: Seq[String]): (String, String) =

    val components = inputLines.collect:
      case ComponentExtractor(cmpnt) => cmpnt

    val (_, resultPart2, resultPart1) = findBest3(components.toList)



    val result1 = s"$resultPart1"
    val result2 = s"$resultPart2"

    (s"${result1}", s"${result2}")

end Solution

case class Component(sideOne: Int, sideTwo: Int):
  lazy val value: Int = sideOne + sideTwo

case class Bridge2(head: Component, tail: Option[Bridge2]):
  private lazy val openPin: Int =
    tail match
      case None => head.sideOne match
        case 0 => head.sideTwo
        case value => value
      case Some(includedBridge) =>
        includedBridge.openPin match
          case value if value == head.sideOne => head.sideTwo
          case _ => head.sideOne

  lazy val length: Int =
    tail match
      case None => 1
      case Some(bridge) => 1 + bridge.length
  lazy val headComponent: Component = head
  lazy val headValue: Int = headComponent.value
  lazy val value: Int = headComponent.value + tail.map(_.value).getOrElse(0)

  def connect(component: Component): Option[Bridge2] =
    (component.sideOne == openPin, component.sideTwo == openPin) match
      case (false, false) => None
      case _ => Some(Bridge2(component, Some(this)))

case class Bridge(components: List[Component]):
  private lazy val openPin: Int =
    components match
      case Nil => 0
      case head :: Nil => head.sideOne match
        case 0 => head.sideTwo
        case value => value
      case head :: tail =>
        Bridge(tail).openPin match
          case value if value == head.sideOne => head.sideTwo
          case _ => head.sideOne

  lazy val length: Int = components.length
  lazy val headComponent: Option[Component] = components.headOption
  lazy val headValue: Option[Int] = headComponent.map(_.value)

  def connect(component: Component): Option[Bridge] =
    (component.sideOne == openPin, component.sideTwo == openPin) match
      case (false, false) => None
      case _ => Some(Bridge(component +: components))


type BridgeData = (Int, Int)
object Part1Ordering extends Ordering[BridgeData]:
  def compare(a: BridgeData, b: BridgeData) = a._2.compare(b._2)

object Part2Ordering extends Ordering[BridgeData]:
  def compare(a: BridgeData, b: BridgeData) =
    a._1.compare(b._1) match
      case 0 => a._2.compare(b._2)
      case value => value

def findBest(bridges: List[Component], bridge: Bridge = Bridge(Nil), currentMax: Int = 0)(using Ordering[BridgeData]): BridgeData =
  def nextPluggables: List[Bridge] = bridges.flatMap(bridge.connect)
  nextPluggables match
    case Nil => (bridge.length, currentMax)
    case value =>
      value.map:
        currentBridge => findBest(bridges.filterNot(_ == currentBridge.headComponent.get), currentBridge, currentMax + currentBridge.headValue.get)
      .max

def findBest2(bridges: List[Component], bridge: Option[Bridge2] = None, currentMax: Int = 0)(using Ordering[BridgeData]): BridgeData =
  def nextPluggables: List[Bridge2] =
    bridge match
      case None => bridges.filter(c => c.sideOne == 0 || c.sideTwo == 0).map(Bridge2(_, None))
      case Some(bridge) => bridges.flatMap(bridge.connect)
  nextPluggables match
    case Nil => (bridge.map(_.length).getOrElse(0), currentMax)
    case value =>
      value.map:
        currentBridge => findBest2(bridges.filterNot(_ == currentBridge.headComponent), Some(currentBridge), currentMax + currentBridge.headValue)
      .max


def findBest3(bridges: List[Component], bridge: Option[Bridge2] = None): (Int, Int, Int) =
  def nextPluggables: List[Bridge2] =
    bridge match
      case None => bridges.filter(c => c.sideOne == 0 || c.sideTwo == 0).map(Bridge2(_, None))
      case Some(bridge) => bridges.flatMap(bridge.connect)
  nextPluggables match
    case Nil => (bridge.map(_.length).getOrElse(0), bridge.map(_.value).getOrElse(0), bridge.map(_.value).getOrElse(0))
    case value =>
      value.foldLeft((0, 0, 0)):
        case ((length, value1, value2), currentBridge) =>
          val (newLength, newValue1, newValue2) = findBest3(bridges.filterNot(_ == currentBridge.headComponent), Some(currentBridge))
          if (newLength == length)
            (newLength, math.max(value1, newValue1), math.max(value2, newValue2))
          else if (newLength > length)
            (newLength, newValue1, math.max(value2, newValue2))
          else
            (length, value1, math.max(value2, newValue2))

object ComponentExtractor:
  val extractor = """(\d+)/(\d+)""".r
  def unapply(str: String): Option[Component] =
    extractor.findAllMatchIn(str).map(_.subgroups).toList.headOption.map(_.map(_.toInt)) match
      case Some(List(a,b)) => Some(Component(a, b))
      case _ => None

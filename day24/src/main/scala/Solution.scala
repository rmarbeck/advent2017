object Solution:
  def run(inputLines: Seq[String]): (String, String) =

    val components = inputLines.collect:
      case ComponentExtractor(cmpnt) => cmpnt

    val (_, resultPart2, resultPart1) = findBest(components.toList)

    val result1 = s"$resultPart1"
    val result2 = s"$resultPart2"

    (s"${result1}", s"${result2}")

end Solution

case class Component(sideOne: Int, sideTwo: Int):
  lazy val value: Int = sideOne + sideTwo
  def canConnect(pin: Int): Boolean = sideOne == pin || sideTwo == pin

case class Bridge(head: Component, tail: Option[Bridge] = None):
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

  def enlarge(component: Component): Option[Bridge] =
    (component.sideOne == openPin, component.sideTwo == openPin) match
      case (false, false) => None
      case _ => Some(Bridge(component, Some(this)))

def findBest(bridges: List[Component], bridge: Option[Bridge] = None): (Int, Int, Int) =
  def nextPossibleBridges: List[Bridge] =
    bridge match
      case None => bridges.filter(_.canConnect(0)).map(Bridge(_))
      case Some(bridge) => bridges.flatMap(bridge.enlarge)
  nextPossibleBridges match
    case Nil =>
      bridge match
        case Some(finalBridge) => (finalBridge.length, finalBridge.value, finalBridge.value)
        case None => throw Exception("Should not happen")
    case enlargedBridges =>
      enlargedBridges.foldLeft((0, 0, 0)):
        case ((length, valuePart2, valuePart1), currentBridge) =>
          val (newLength, newValuePart2, newValuePart1) = findBest(bridges.filterNot(_ == currentBridge.headComponent), Some(currentBridge))
          val (bestLength, bestValuePart2) =
            newLength - length match
              case 0 => (length, math.max(valuePart2, newValuePart2))
              case value if value > 0 => (newLength, newValuePart2)
              case _ => (length, valuePart2)

          (bestLength, bestValuePart2, math.max(valuePart1, newValuePart1))

object ComponentExtractor:
  val extractor = """(\d+)/(\d+)""".r
  def unapply(str: String): Option[Component] =
    extractor.findAllMatchIn(str).map(_.subgroups).toList.headOption.map(_.map(_.toInt)) match
      case Some(List(a,b)) => Some(Component(a, b))
      case _ => None


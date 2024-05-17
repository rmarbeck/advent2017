object Solution:
  def run(inputLines: Seq[String]): (String, String) =


    val bridges = inputLines.collect:
      case Bridge(a, b) => Bridge(a, b)

    bridges.toList.foreach(println)

    println(findBest(bridges.toList, Nil, 0))



    val result1 = s""
    val result2 = s""

    (s"${result1}", s"${result2}")

end Solution

case class Bridge(sideOne: Int, sideTwo: Int):
  lazy val turned = Bridge(sideTwo, sideOne)
  lazy val value: Int = sideOne + sideTwo
  def canPlug(pin: Int): Boolean = sideOne == pin
  def canPlug(other: Bridge): Boolean = canPlug(other.sideTwo)

def findBest(bridges: List[Bridge], alreadyInBridge: List[Bridge], currentMax: Int): Int =
  def nextPluggables: List[Bridge] =
    alreadyInBridge match
      case Nil => bridges.filter(_.canPlug(0))
      case head :: tail => bridges.filter(_.canPlug(head))
  nextPluggables match
    case Nil =>
      println(s"${alreadyInBridge} $currentMax")
      currentMax
    case value =>
      value.map:
        currentBridge => findBest(bridges.filterNot(_ == currentBridge), currentBridge +: alreadyInBridge, currentMax + currentBridge.value)
      .max


object Bridge:
  val extractor = """(\d+)/(\d+)""".r
  def unapply(str: String): Option[Bridge] =
    extractor.findAllMatchIn(str).map(_.subgroups).toList.headOption.map(_.map(_.toInt)) match
      case Some(List(a,b)) => Some(Bridge(a, b))
      case _ => None

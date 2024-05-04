object Solution:
  def run(inputLines: Seq[String]): (String, String) =

    val actions = inputLines.head.split(",").collect:
      case s"s$value" => Spin(value.toInt)
      case s"x$valueA/$valueB" => Exchange(valueA.toInt, valueB.toInt)
      case s"p$programA/$programB" => Partner(programA(0), programB(0))

    val start = actions.length match
      case 3 => ('a' to 'e').foldLeft("")(_ + _)
      case _ => ('a' to 'p').foldLeft("")(_ + _)

    val resultPart1 = dance(start, actions)

    val resultPart2 = findValueAt(start, actions, 1_000_000_000)

    val result1 = s"$resultPart1"
    val result2 = s"$resultPart2"

    (s"${result1}", s"${result2}")

end Solution

def findValueAt(start: String, actions: Array[Action], at: Int): String =
  val (init, wideness, values) = findFirstRepeated(start, actions)
  at < init match
    case true => throw Exception("Not supported")
    case false =>
      values((at - init) % wideness)

def findFirstRepeated(start: String, actions: Array[Action]): (Int, Int, Array[String]) =
  def process(current: String, step: Int = 0, previous: Map[String, Int]): (Int, Int, Map[String, Int]) =
    previous.get(current) match
      case None =>
        process(dance(current, actions), step + 1, previous + (current -> step))
      case Some(initial) => (initial, step - initial, previous)

  val (init, wideness, values) = process(start, previous = Map())
  (init, wideness, values.toList.sortBy(_._2).map(_._1).toArray)

def dance(current: String, actions: Array[Action]): String =
  actions.foldLeft(current):
    case (acc, action: Action) => action(acc)

trait Action:
  def apply(str: String): String

case class Spin(position: Int) extends Action:
  override def apply(str: String): String =
    s"${str.takeRight(position)}${str.take(str.length-position)}"

case class Exchange(positionA: Int, positionB: Int) extends Action:
  override def apply(str: String): String =
    val current = str.toCharArray
    val initA = current(positionA)
    current(positionA) = current(positionB)
    current(positionB) = initA
    current.mkString

case class Partner(programA: Char, programB: Char) extends Action:
  override def apply(str: String): String =
    val positionA = str.indexOf(programA)
    val positionB = str.indexOf(programB)
    Exchange(positionA, positionB).apply(str)


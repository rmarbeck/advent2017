object Solution:
  def run(inputLines: Seq[String]): (String, String) =

    val LineExtractor = """([^ ]+) \((\d+)\)(.*)?""".r.anchored
    val childExtractor = """([a-z]+)""".r.unanchored

    val programs = inputLines.collect:
      case LineExtractor(root, size, childrenAsString) =>
        val children = childrenAsString match
          case null => Nil
          case value => childExtractor.findAllMatchIn(value).map(_.matched).toList
        Program(root, size.toInt, children)

    val tower = Tower(programs)
    val resultPart1 = tower.root match
      case Right(result) => result.name
      case Left(error) => error

    given Tower = tower
    val ranked = programs.map:
      prg => prg.name -> Ranked(name = prg.name, load = prg.load, initialSize = prg.size)
    .toMap

    val resultPart2 =  find(resultPart1, ranked, 0)

    val result1 = s"$resultPart1"
    val result2 = s"$resultPart2"

    (s"${result1}", s"${result2}")

end Solution

def find(current: String, ranks: Map[String, Ranked], difference: Int)(using tower: Tower): Int =
  tower.next(current) match
    case Nil => ranks(current).initialSize - difference
    case children =>
      val grouped = children.groupMap(ranks(_).load)(identity)
      grouped.find(_._2.length == 1) match
        case None => ranks(current).initialSize - difference
        case Some(singleChild) =>
          val singleChildLoad = singleChild._1
          val othersLoad = grouped.keys.filterNot(_ == singleChildLoad).head
          find(singleChild._2.head, ranks, singleChildLoad - othersLoad)



type Genealogy = (Option[String], List[String])

case class Ranked(name: String, load: Int, initialSize: Int)

case class Program(name: String, size: Int, children: List[String]):
  def load(using tower: Tower): Int =
    size + children.foldLeft(0):
      (acc, child) => acc + tower.byName(child).load

case class Tower(programs: Seq[Program]):
  private val hierarchy: Map[Program, Genealogy] =
    val firstPass: Map[String, List[String]] = programs.map(p => p.name -> p.children).toMap
    programs.map(p => p -> (firstPass.find(_._2.contains(p.name)).map(_._1), p.children)).toMap

  private def internalByName(n: String): Option[Program] = programs.find(_.name == n)

  def byName(name: String): Program = internalByName(name).get
  def next(name: String): List[String] = internalByName(name).map(_.children).get

  lazy val root: Either[String, Program] =
    val found = hierarchy.find:
      case (key, value) => value._1.isEmpty

    found match
      case None => Left("Not found")
      case Some(value) => Right(value._1)


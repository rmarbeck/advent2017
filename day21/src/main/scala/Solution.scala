import scala.collection.immutable.ListMap

object Solution:
  def run(inputLines: Seq[String]): (String, String) =

    val rules = Rules {
      inputLines.collect:
       case RuleExtractor(rule) => rule
    }

    given Rules = rules

    println(Image.default)

    println(Image.default.enhance)

    println(Image.default.enhance.enhance)


    val result1 = s""
    val result2 = s""

    (s"${result1}", s"${result2}")

end Solution

type Pixels = Array[Array[Boolean]]

case class Rule(input: Pixels, output: Pixels):
  lazy val size = input.size
  private def isEqualTo(pixels: Pixels): Boolean =
    pixels.flatten.zip(input.flatten).forall:
      case (one, other) => one == other
  private def matches(toCompare: Image): Boolean =
    toCompare.size == size match
      case false => false
      case true => toCompare.variants.exists(isEqualTo)

  def enhance(image: Image): Option[Image] = matches(image) match
    case true => Some(Image(output))
    case false => None

object RuleExtractor:
  def unapply(str: String): Option[Rule] =
    def toArrayOfBoolean(rawStr: String): Pixels =
      rawStr.split("/").map:
        _.toCharArray.map:
          case '#' => true
          case '.' => false
    str match
      case s"$input => $output" =>
        val inputs = toArrayOfBoolean(input)
        val outputs = toArrayOfBoolean(output)
        Some(Rule(inputs, outputs))
      case _ => None

case class Rules(rules: Seq[Rule]):
  lazy val rulesFor2By2: Seq[Rule] = rules.filter(_.size == 2)
  lazy val rulesFor3By3: Seq[Rule] = rules.filter(_.size == 3)

case class Image(pixels: Pixels):
  override def toString: String =
    pixels.map:
      line => line.map:
        case true => '#'
        case false => '.'
      .mkString
    .mkString("\n")


  lazy val size = pixels.length
  lazy val variants: Iterator[Pixels] = nextVariant(pixels, 0).iterator

  private def extract(toSize: Int): List[Image] =
    val grouped =
      for
        row <- pixels.indices
      yield
        pixels(row).grouped(toSize)

    val newSize = pixels.length / toSize

    val asMap = grouped.zipWithIndex.groupMapReduce(_._2 % newSize)(_._1.toList)(_ ::: _)
    val sortedValues = ListMap[Int, List[Array[Boolean]]](asMap.toSeq.sortBy(_._1): _*)
    sortedValues.values.flatMap(_.grouped(toSize).map(_.toArray)).map(Image.apply).toList
    //sortedValues.values.map(_.toArray).map(Image.apply).toList

  def as2By2: List[Image] = extract(2)

  def as3By3: List[Image] = extract(3)

  def enhance(using Rules): Image =
    size match
      case 2 => summon[Rules].rulesFor2By2.flatMap(_.enhance(this)).headOption.getOrElse(this)
      case 3 => summon[Rules].rulesFor3By3.flatMap(_.enhance(this)).headOption.getOrElse(this)
      case _ =>
        (size % 2 == 0, size % 3 == 0) match
          case (true, _) =>
            val test = as2By2
            Image.collate(as2By2.map(_.enhance))
          case (_, true) => Image.collate(as3By3.map(_.enhance))
          case _ => this

object Image:
  lazy val default: Image =
    Image {
      Array(
        Array(false, true, false),
        Array(false, false, true),
        Array(true, true, true)
      )
    }
  def collate(parts: List[Image]): Image =
    val subImagesCount = parts.size
    val groupSize = Math.sqrt(subImagesCount).toInt
    val arrays = parts.grouped(groupSize).toArray.map(_.toArray)
    Image {
      Array.tabulate(subImagesCount, subImagesCount):
        case (row, col) => arrays(row / groupSize)(col / groupSize).pixels(row % groupSize)(col % groupSize)
    }

def nextVariant(pixels: Pixels, rank: Int): LazyList[Pixels] =
  def rotate(pixels: Pixels): Pixels =
    pixels.transpose
  def flip(pixels: Pixels): Pixels =
    pixels.map(_.reverse)
  rank >= 8 match
    case true => LazyList.empty
    case false =>
      val current = pixels
      val nextOne = rank % 2 == 0 match
        case true => rotate(pixels)
        case false => flip(pixels)
      current #:: nextVariant(nextOne, rank + 1)
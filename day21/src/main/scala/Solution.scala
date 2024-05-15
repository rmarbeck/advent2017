import scala.annotation.tailrec

object Solution:
  def run(inputLines: Seq[String]): (String, String) =

    val rules = Rules {
      inputLines.collect:
       case RuleExtractor(rule) => rule
    }

    given Rules = rules

    val part1Image = doEnhance(Image.default, 5)
    val part2Image = doEnhance(part1Image, 13)

    val result1 = s"${part1Image.on}"
    val result2 = s"${part2Image.on}"

    (s"${result1}", s"${result2}")

end Solution

@tailrec
def doEnhance(image: Image, nTimes: Int)(using Rules): Image =
  require(nTimes >= 0)
  nTimes match
    case 0 => image
    case _ => doEnhance(image.enhance, nTimes - 1)

type Pixels = Array[Array[Boolean]]

case class Rule(input: Pixels, output: Pixels):
  lazy val size = input.size
  private lazy val inputFlattened = input.flatten 
  private def isEqualTo(pixels: Pixels): Boolean =
    pixels.flatten.zip(inputFlattened).forall:
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

  import scala.collection.mutable.{Map, HashMap}
  val cached: Map[String, Image] = HashMap()
  def enhance(image: Image): Image =
    cached.getOrElseUpdate(image.forHashing, {
      image.size match
        case 2 => rulesFor2By2.flatMap(_.enhance(image)).headOption.getOrElse(image)
        case 3 => rulesFor3By3.flatMap(_.enhance(image)).headOption.getOrElse(image)
        case _ => throw Exception("Not supported")
    })

case class Image(pixels: Pixels):
  lazy val on: Int = pixels.flatten.count(_ == true)
  def safeCopy: Image =
    Image {
      pixels.map(_.map(identity))
    }

  lazy val forHashing: String = pixels.flatten.map:
    case true => '1'
    case false => '0'
  .mkString

  override def toString: String =
    pixels.map:
      line => line.map:
        case true => '#'
        case false => '.'
      .mkString
    .mkString("\n")

  lazy val size = pixels.length
  def variants: Iterator[Pixels] = nextVariant(pixels, 0).iterator

  private def extract(toSize: Int): List[Image] =
    val newSize = pixels.length / toSize
    pixels.grouped(toSize).flatMap:
      line => line.transpose.grouped(toSize).map(_.transpose).map(Image.apply)
    .toList

  def as2By2: List[Image] = extract(2)

  def as3By3: List[Image] = extract(3)

  def enhance(using Rules): Image =
    size match
      case 2 | 3 =>
        summon[Rules].enhance(this)
      case _ =>
        (size % 2 == 0, size % 3 == 0) match
          case (true, _) => Image.collate(as2By2.map(_.enhance))
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
    val subImagesSize = parts(0).size
    val subImagesPerLine = Math.sqrt(parts.size).toInt
    val imagesGroupedByLines = parts.grouped(subImagesPerLine).toArray
    val finalSize = subImagesSize * subImagesPerLine
    Image {
      Array.tabulate(finalSize, finalSize):
        case (row, col) => imagesGroupedByLines(row / subImagesSize)(col / subImagesSize).pixels(row % subImagesSize)(col % subImagesSize)
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
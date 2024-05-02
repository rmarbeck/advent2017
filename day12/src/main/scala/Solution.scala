import scala.collection.immutable.BitSet
import scala.collection.mutable

object Solution:
  def run(inputLines: Seq[String]): (String, String) =

    val programsExtractor = """(\d+)""".r.unanchored
    val groupsHolder = GroupsHolder()
    inputLines.foreach:
      case line =>
        groupsHolder.digest(programsExtractor.findAllIn(line).map(_.toInt).toList)

    val resultPart1 = groupsHolder.containing(0).size
    val resultPart2 = groupsHolder.nbGroups

    val result1 = s"$resultPart1"
    val result2 = s"$resultPart2"

    (s"${result1}", s"${result2}")

end Solution

class GroupsHolder :
  import scala.collection.mutable.Map
  private val groups: mutable.Map[Int, Group] = mutable.Map()

  def digest(newMembers: List[Int]): Unit =
    val existingGroups = newMembers.flatMap(groups.get)
    val updatedGroup = existingGroups.foldLeft(Group.from(newMembers)):
      case (acc, newGroup) => acc.addAll(newGroup.members)

    updatedGroup.members.foreach:
      member => groups.update(member, updatedGroup)

  def containing(program: Int): Group = groups(program)
  def nbGroups: Int = groups.values.toSet.size

class Group(var members: BitSet):
  override def toString: String = members.mkString(",")
  def size: Int = members.size
  def addAll(newMembers: Set[Int]): Group =
    members = members ++ newMembers
    this
  def add(newMember: Int): Group =
    members = members + newMember
    this

object Group:
  def from(newMembers: List[Int]): Group = Group(BitSet.empty ++ newMembers.toSet)
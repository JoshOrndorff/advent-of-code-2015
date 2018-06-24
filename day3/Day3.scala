// https://stackoverflow.com/questions/50632210

object Day3 {

  val input = io.Source.fromFile("input.txt")
  val directions = try input.mkString.toList finally input.close()

  def part1(visited: Set[Coord], current: Coord, directions: List[Char]):  Set[Coord] =
    directions match {
      case '^' :: tail => part1(visited + current, current.up, tail)
      case 'v' :: tail => part1(visited + current, current.down, tail)
      case '<' :: tail => part1(visited + current, current.left, tail)
      case '>' :: tail => part1(visited + current, current.right, tail)
      case _ => visited + current // The string is empty
    }

  def main(args: Array[String]) = {
    // Part 1: How many houses did Santa visit?
    def visited = part1(Set.empty[Coord], new Coord(0, 0), directions)
    println("Part 1: Santa visited " + visited.size + " houses.")

    // Part 2: How many houses did Santa and Robo-Santa visit?
    // Idea for zipWithIndex from:
    // https://www.reddit.com/r/adventofcode/comments/3v8roh/day_3_solutions/cxlhg3j/
    val santaDirections = directions.zipWithIndex.filter(x => x._2 % 2 == 0).map(x => x._1)
    val roboDirections  = directions.zipWithIndex.filter(x => x._2 % 2 != 0).map(x => x._1)
    val santaVisited = part1(Set.empty[Coord], new Coord(0, 0), santaDirections)
    val roboVisited  = part1(Set.empty[Coord], new Coord(0, 0), roboDirections)
    println("Part 2: Santa and Robo-Santa visited " + (santaVisited ++ roboVisited).size + " houses.")
  }
}

class Coord(ex: Int, wy: Int){ //TODO how to use duplicate names like in java?
  def x = ex
  def y = wy

  def up   : Coord = new Coord(x, y + 1)
  def down : Coord = new Coord(x, y - 1)
  def left : Coord = new Coord(x - 1, y)
  def right: Coord = new Coord(x + 1, y)

  // Interesting: x.hashCode + y.hashCode << 1 still had collisions.
  // Maybe I really shouldn't roll my own.
  override def hashCode = (x + ", " + y).hashCode
  override def equals(other: Any) = this.hashCode == other.hashCode
}

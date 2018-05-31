// https://stackoverflow.com/questions/50632210

object Day3 {

  val input = io.Source.fromFile("input.txt")
  val directions = try input.mkString finally input.close()

    def part1(visited: Set[Coord], current: Coord, directions: String):  Set[Coord] = directions match {
      case x if x.startsWith("^") => part1(visited + current, current.up, x.tail)
      case x if x.startsWith("v") => part1(visited + current, current.down, x.tail)
      case x if x.startsWith("<") => part1(visited + current, current.left, x.tail)
      case x if x.startsWith(">") => part1(visited + current, current.right, x.tail)
      case _ => visited + current // The string is empty
    }

  def main(args: Array[String]) = {
    // Part 1: How many houses did Santa visit?
    def visited = part1(Set.empty[Coord], new Coord(0, 0), directions)
    println("Part 1: Santa visited " + visited.size + " houses.")

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

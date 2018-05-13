
object Day2 {

  val presentStrings = io.Source.fromFile("input.txt").getLines
  val presents = presentStrings.map(_.split("x").map(_.toInt))
  // File parsing correctly. I have a 2D Array.

  val test1 = Array(2,3,4)
  val test2 = Array(1,1,10)

  /**
   * Computes the paper needed for one present.
   * Part 1
   */
  def paper(present: Array[Int]): Int = {
    // Reminder: Lists are linked lists, and access in O(n). Luckily presents are short.
    val dims = List(present(0) * present(1), present(1) * present(2), present(0) * present(2))
    def min(lowest: Int, xs: List[Int]): Int = {
      if (xs.isEmpty) lowest
      else if (xs.head < lowest) min (xs.head, xs.tail)
      else min ( lowest, xs.tail)
    }
    2 * dims.sum + min(dims(0), dims)
  }

  // Print results
  def main(args: Array[String]) = {
    println("Test1 paper: " + paper(test1))
    println("Test2 paper : " + paper(test2))
    println("Part 1: Total paper needed: " + presents.map(paper).reduceLeft(_ + _))
  }

}

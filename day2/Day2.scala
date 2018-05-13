
object Day2 {

  val presentStrings = io.Source.fromFile("input.txt").getLines.toList
  val presents = presentStrings.map(_.split("x").map(_.toInt))
  // File parsing correctly. I have a 2D List.

  val test1 = Array(2,3,4)
  val test2 = Array(1,1,10)

  /**
   * Computes the extreme of a list (min or max in this case)
   */
  def ext(leader: Int, xs: Array[Int], f: (Int, Int) => Boolean): Int = {
   if (xs.isEmpty) leader
   else if (f(xs.head, leader)) ext(xs.head, xs.tail, f)
   else ext(leader, xs.tail, f)
  }


  /**
   * Computes the paper needed for one present.
   * Part 1
   */
  def paper(present: Array[Int]): Int = {
    val dims = Array(present(0) * present(1), present(1) * present(2), present(0) * present(2))

    2 * dims.sum + ext(dims(0), dims, _ < _)
  }

  /**
   * Computes the ribbon needed for one present.
   * Part 2
   */
  def ribbon(present: Array[Int]): Int = {
    2 * present.sum - 2 * ext(present(0), present, _ > _) + present.reduceLeft(_ * _)
  }

  // Print results
  def main(args: Array[String]) = {
    println("Test1 paper: " + paper(test1))
    println("Test2 paper: " + paper(test2))
    println("Part 1: Total paper needed: " + presents.map(paper).reduceLeft(_ + _))
    println()
    println("Test1 ribbon: " + ribbon(test1))
    println("Test2 ribbon: " + ribbon(test2))
    println("Part 2: Total ribbon needed: " + presents.map(ribbon).reduceLeft(_ + _))
  }

}

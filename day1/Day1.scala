
object Day1 {

  val input = io.Source.fromFile("input.txt")
  val instructions = try input.mkString finally input.close()

  // Part 1
  def endingFloor(inst: String, acc: Int): Int = {
    if (inst == "") acc
    else if (inst.substring(0, 1) == "(") endingFloor(inst.substring(1), acc + 1)
    else endingFloor(inst.substring(1), acc - 1)
  }

  // Part 2
  def firstBasement(inst: String, acc: Int, pos: Int): Int = {
    if (acc == -1) pos
    else if (inst == "") -1 // Something went wrong. Instructions ended without hitting basement.
    else if (inst.substring(0, 1) == "(") firstBasement(inst.substring(1), acc + 1, pos + 1)
    else firstBasement(inst.substring(1), acc - 1, pos + 1)
  }


  // Print results
  def main(args: Array[String]) = {
    println("Part 1: Santa ends on floor: " + endingFloor(instructions, 0))
    println("Part 2: Santa hits basement: " + firstBasement(instructions, 0, 0))
  }

}

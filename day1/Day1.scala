// not -7000
//not -280

object Day1 {

  val input = io.Source.fromFile("input.txt")
  val instructions = try input.mkString finally input.close()

  def countFloors(inst: String): Int = {
    def countRec(inst: String, acc: Int):Int = {
      if (inst == "") acc
      else if (inst.substring(0, 1) == "(") countRec(inst.substring(1), acc + 1)
      else countRec(inst.substring(1), acc - 1)
    }
    countRec(inst, 0)
  }

  def main(args: Array[String]) = println(countFloors(instructions))

}

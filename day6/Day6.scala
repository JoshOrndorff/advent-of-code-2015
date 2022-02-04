object Day6 extends App {

  // Read the file
  val input = ioSource.fromFile("input.txt")
  val instrs =
    try input.toList.map(x => Instruction(x))
    finally input.close()

  /**
   * Recurses through instructions updating a grid
   */
  def configLights(lights: List[List[Boolean]], instrs: List[Instruction]): List[List[Boolean]] =
    if (instrs.size == 0) acc
    else
      //TODO apply


  // Part 1
}

class Instruction {
  //TODO
}

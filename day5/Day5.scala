object Day5 extends App{

  // Read file into a 2D list (strings and characters)
  val input = io.Source.fromFile("input.txt")
  val strs =
    try input.getLines.toList.map(_.toList)
    finally input.close()

  // Run the tests
  println(isNice("ugknbfddgicrmopn".toList))
  println(isNice("aaa".toList))
  println(isNice("jchzalrnumimnmhp".toList))
  println(isNice("haegwjzuvuyypxyu".toList))
  println(isNice("dvszwmarrgswjxmb".toList))

  // Print result
  println(strs.map(isNice(_)).filter(x => x).size)

  def isNice(s: List[Char]): Boolean =
    threeVowels(s) && doubles(s) && !forbidden(s)

  def threeVowels(s: List[Char]): Boolean = countVowels(0, s) >= 3

  def countVowels(acc: Int, s: List[Char]): Int = s match {
    case Nil => acc
    case 'a' :: tail => countVowels(acc + 1, s.tail)
    case 'e' :: tail => countVowels(acc + 1, s.tail)
    case 'i' :: tail => countVowels(acc + 1, s.tail)
    case 'o' :: tail => countVowels(acc + 1, s.tail)
    case 'u' :: tail => countVowels(acc + 1, s.tail)
    case _ => countVowels(acc, s.tail)
  }

  def doubles(s: List[Char]): Boolean = s match {
    case _ :: Nil => false
    // TODO there must be a way to pattern match here
    // maybe custom matcher?
    case a :: b :: tail => if (a == b) true else doubles(b :: tail)
    case _ => throw new Error("Double Trouble")
  }

  def forbidden(s: List[Char]): Boolean = s match {
    case 'a' :: 'b' :: _ => true
    case 'c' :: 'd' :: _ => true
    case 'p' :: 'q' :: _ => true
    case 'x' :: 'y' :: _ => true
    case _ :: tail => forbidden(tail)
    case _ => false
  }
}

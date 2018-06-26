object Day5 extends App{

  // Read file into a 2D list (strings and characters)
  val input = io.Source.fromFile("input.txt")
  val strs =
    try input.getLines.toList.map(_.toList)
    finally input.close()

  // Run the tests
  println(isNice1("ugknbfddgicrmopn".toList))
  println(isNice1("aaa".toList))
  println(isNice1("jchzalrnumimnmhp".toList))
  println(isNice1("haegwjzuvuyypxyu".toList))
  println(isNice1("dvszwmarrgswjxmb".toList))
  println()
  println(isNice2("qjhvhtzxzqqjkmpb".toList))
  println(isNice2("xxyxx".toList))
  println(isNice2("uurcxstgmygtbstg".toList))
  println(isNice2("ieodomkazucvgmuy".toList))
  println()

  // Print result
  println(strs.filter(isNice1).size)
  println(strs.filter(isNice2).size)

  // Part 1 Functions
  def isNice1(s: List[Char]): Boolean =
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

  // Part 2 Functions
  def isNice2(s: List[Char]): Boolean =
    dupePair(s) && sandwich(s)

  def dupePair(s: List[Char]): Boolean =
    if (s.size < 4) false
    else if (containsPair(s.drop(2), s.slice(0, 2))) true
    else dupePair(s.tail)

  def containsPair(haystack: List[Char], needle: List[Char]): Boolean =
    if (haystack.size < 2) false
    else if (haystack.slice(0, 2) == needle) true
    else containsPair(haystack.tail, needle)

  // @tailrec // Not sure why annotation isn't working
  def sandwich(s: List[Char]): Boolean = s match {
    case Nil => false
    case _ :: Nil => false
    case _ :: _ :: Nil => false
    case a :: b :: c :: tail => if (a == c) true else sandwich(b :: c :: tail)
  }

}

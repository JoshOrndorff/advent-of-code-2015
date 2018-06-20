// https://gist.github.com/amscotti/2467512 for generating MD5
import java.security.MessageDigest

object Day4 extends App {

  val digest = MessageDigest.getInstance("MD5")

  def md5(text: String): String =
    digest.digest(text.getBytes).map("%02x".format(_)).mkString

  def findNonce(key: String, diff: Int): Int = {
    def findNonceRecurse(n: Int): Int = {
      if (md5(key + n).substring(0, diff) == ("%0"+diff+"d").format(0)) n
      else findNonceRecurse(n + 1)
    }

    findNonceRecurse(0)
  }

  // Run the tests
  //println(findNonce("abcdef", 5))
  //println(findNonce("pqrstuv", 5))

  // Part 1
  println(findNonce("yzbqklnj", 5))

  // Part 2
  println(findNonce("yzbqklnj", 6))
}

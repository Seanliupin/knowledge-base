package helper

/**
  * Author: Sean
  * Date: 5/10/2017
  * Time: 12:09 PM
  */
object StringUtil {
  val whiteSpaceSegmenter = "[\\s]+"

  def isBlank(str: String): Boolean = str.trim.length == 0

  final def isNotBlank(str: String): Boolean = !isBlank(str)
}

package model.html

/**
  * Author: Sean
  * Date: 4/10/2017
  * Time: 11:45 PM
  */
case class Node(tag: String, value: String) {
  var names: List[String] = List()

  def className(name: String): Node = {
    names = name :: names
    this
  }

  override def toString(): String = {
    val re = names.mkString(", ")
    s"<$tag $re>$value</$tag>"
  }
}

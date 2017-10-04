package model.html

/**
  * Author: Sean
  * Date: 4/10/2017
  * Time: 11:45 PM
  */
case class Node(tag: String, value: String) {
  var names: List[String] = List()
  private var title = ""

  def className(name: String): Node = {
    names = name :: names
    this
  }

  def title(title: String): Node = {
    this.title = title
    this
  }

  def href(ref: String): Node = {
    //todo
    this
  }

  override def toString(): String = {
    var className = ""
    if (names.size > 0) {
      className = names.mkString(", ")
      className = "class =" + "\"" + className + "\""
    }

    var myTitle = ""
    if (title.trim.size > 0) {
      myTitle = "title= " + "\"" + title + "\""
    }


    s"<$tag $className $myTitle>$value</$tag>"
  }
}

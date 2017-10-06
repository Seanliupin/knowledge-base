package model.html

/**
  * Author: Sean
  * Date: 4/10/2017
  * Time: 11:45 PM
  */
case class Node(tag: String, value: String) {
  var names: List[String] = List()
  private var title = ""
  private var href = ""

  def className(name: String): Node = {
    names = name :: names
    this
  }

  def title(title: String): Node = {
    this.title = title
    this
  }

  def href(href: String): Node = {
    this.href = href
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

    var myHref = ""
    if (href.trim.size > 0) {
      myHref = "href= " + "\"" + href + "\""
    }

    s"<$tag $className $myTitle $myHref>$value</$tag>"
  }
}

object Node {
  implicit def nodeToString(node: Node): String = node.toString()
}

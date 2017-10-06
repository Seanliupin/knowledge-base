package model.html

/**
  * Author: Sean
  * Date: 4/10/2017
  * Time: 11:45 PM
  */
case class Node(tag: String, value: String) {
  var names: List[String] = List()
  private var properties: List[(String, String)] = List()
  private var title = ""
  private var href = ""
  private var nodeText: String = value

  def className(name: String): Node = {
    names = name :: names
    this
  }

  def addProperty(name: String, value: String): Node = {
    properties = (name, value) +: properties
    this
  }

  def setText(text: String): Node = {
    nodeText = text
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
      className = names.mkString(" ")
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

    var re = ""
    properties.foreach(p => {
      re += p._1 + "=\"" + p._2 + "\"  "
    })

    s"<$tag $className $myTitle $myHref $re>$nodeText</$tag>"
  }
}

object Node {
  implicit def nodeToString(node: Node): String = node.toString()
}

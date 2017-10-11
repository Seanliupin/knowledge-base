package model.html

/**
  * Author: Sean
  * Date: 4/10/2017
  * Time: 11:45 PM
  */
case class Node(tag: Option[String], value: String) {
  var names: List[String] = List()
  private var properties: List[(String, String)] = List()
  private var title = ""
  private var href = ""
  private var nodeText: String = value
  private var outerNode: Option[Node] = None

  def className(name: String): Node = {
    names = name :: names
    this
  }

  def addProperty(name: String, value: String): Node = {
    properties = (name, value) +: properties
    this
  }

  def setOuterNode(node: Node): Node = {
    outerNode = Some(node)
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
    if (tag == None) {
      return ""
    }
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

    val selfHtml = tag match {
      case Some(tagValue) if tagValue.trim.length > 0 => s"<$tagValue $className $myTitle $myHref $re>$nodeText</$tagValue>"
      case _ => ""
    }

    outerNode match {
      case Some(node) => node.setText(selfHtml).toString()
      case None => selfHtml
    }
  }
}

object Node {
  implicit def nodeToString(node: Node): String = node.toString()

  def emptyNode: Node = {
    Node(None, "")
  }
}

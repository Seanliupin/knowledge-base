package model.html

/**
  * Author: Sean
  * Date: 4/10/2017
  * Time: 11:45 PM
  */
case class HtmlNode(tag: Option[String], value: String) {
  var names: List[String] = List()
  private var properties: List[(String, String)] = List()
  private var title = ""
  private var href = ""
  private var nodeText: String = value
  private var outerNode: Option[HtmlNode] = None

  def className(name: String): HtmlNode = {
    names = name :: names
    this
  }

  def classNames(name: List[String]): HtmlNode = {
    names = name ++ names
    this
  }

  def addProperty(name: String, value: String): HtmlNode = {
    properties = (name, value) +: properties
    this
  }

  def setOuterNode(node: HtmlNode): HtmlNode = {
    outerNode = Some(node)
    this
  }

  def setText(text: String): HtmlNode = {
    nodeText = text
    this
  }

  def title(title: String): HtmlNode = {
    this.title = title
    this
  }

  def href(href: String): HtmlNode = {
    this.href = href
    this
  }

  override def toString(): String = {
    if (tag.isEmpty) {
      return ""
    }
    var className = ""
    if (names.nonEmpty) {
      className = names.mkString(" ")
      className = "class =" + "\"" + className + "\""
    }

    var myTitle = ""
    if (title.trim.length > 0) {
      myTitle = "title= " + "\"" + title + "\""
    }

    var myHref = ""
    if (href.trim.length > 0) {
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

object HtmlNode {
  implicit def nodeToString(node: HtmlNode): String = node.toString()

  def emptyNode: HtmlNode = {
    HtmlNode(None, "")
  }
}

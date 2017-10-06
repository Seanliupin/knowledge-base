package model.html

/**
  * Author: Sean
  * Date: 5/10/2017
  * Time: 12:06 AM
  */
trait Render {
  def toHtml(tokens: List[String]): String

  def toPlain: String

  /**
    * render hit with strong by default, sub class can render hit their style
    **/
  protected def renderHit(text: String, token: String): String = {
    text.replaceAll(token, Node("strong", token).className("text-danger").toString)
  }

  protected def renderHits(text: String, tokens: List[String]): String = {
    var renderedText = text
    tokens.foreach(token => {
      renderedText = renderHit(renderedText, token)
    })
    renderedText
  }

  final def renderHits(tokens: List[String]): String = {
    renderHits(toPlain, tokens)
  }
}

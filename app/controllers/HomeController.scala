package controllers

import javax.inject._

import play.api.db._
import play.api.mvc._
import resource._

/**
  * This controller creates an `Action` to handle HTTP requests to the
  * application's home page.
  */
@Singleton
class HomeController @Inject()(cc: ControllerComponents, db: Database) extends AbstractController(cc) {

  val dir = "/Users/seanliu/Note/"

  def index() = Action { implicit request: Request[AnyContent] =>
    Ok(views.html.index("", ""))
  }

  def search(query: String) = Action { request: Request[AnyContent] =>
    val s = new StringBuilder
    val fileName = dir + "daily/dailyLog-2017-9-10.md"

    val tokens = query.trim.split("[\\s]+")

    def renderHit(text: String, token: String): String = {
      text.replaceAll(token, "<strong class=\"text-danger\">" + token + "</strong>")
    }

    for (source <- managed(scala.io.Source.fromFile(fileName))) {
      for (line <- source.getLines) {
        val contain = tokens.exists(token => {
          line.contains(token)
        })

        if (contain) {
          var renderedLine = line
          tokens.foreach(token => {
            renderedLine = renderHit(renderedLine, token)
          })
          s.append(renderedLine)
          s.append("<br/> <br/>")
        }
      }
    }

    Ok(views.html.index(query, s.toString()))
  }

  def test = TODO
}

package controllers

import javax.inject._

import helper.StringUtil
import play.api.db._
import play.api.mvc._
import service.NoteService

/**
  * This controller creates an `Action` to handle HTTP requests to the
  * application's home page.
  */
@Singleton
class HomeController @Inject()(cc: ControllerComponents, db: Database) extends AbstractController(cc) {
  def index() = Action { implicit request: Request[AnyContent] =>
    Ok(views.html.index("", ""))
  }

  def search(query: String, context: String) = Action { request: Request[AnyContent] =>
    val tokens = query.trim.split(StringUtil.whiteSpaceSegmenter).map(_.trim).filter(_.length > 0).toList
    Ok(views.html.index(query, NoteService.search(tokens, Option(context))))
  }

  def test = TODO
}

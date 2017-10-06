package controllers

import javax.inject._

import helper.StringUtil
import model.note.ContextOption
import play.api.db._
import play.api.mvc._
import service.NoteService

/**
  * This controller creates an `Action` to handle HTTP requests to the
  * application's home page.
  */
@Singleton
class HomeController @Inject()(cc: ControllerComponents, db: Database) extends AbstractController(cc) {

  private val contextList: List[ContextOption] = List(
    ContextOption("all", false),
    ContextOption("body", false),
    ContextOption("keyword", false),
    ContextOption("web", false),
    ContextOption("code", false),
    ContextOption("book", false),
    ContextOption("comment", false)
  )

  def index() = Action { implicit request: Request[AnyContent] =>
    Ok(views.html.index("", contextList.map(c => ContextOption(c.value, c.value == "all")), ""))
  }

  def search(query: String, context: String) = Action { implicit request: Request[AnyContent] =>
    val tokens = query.split(StringUtil.whiteSpaceSegmenter).map(_.trim).filter(_.length > 0).toList
    Ok(views.html.index(query, contextList.map(c => ContextOption(c.value, c.value == context)), NoteService.search(tokens, Option(context))))
  }

  def test = TODO
}

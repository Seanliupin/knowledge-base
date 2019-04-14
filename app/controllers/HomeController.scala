package controllers

import helper.StringUtil
import javax.inject._
import model.note.{ContextOption, Score}
import play.api.db._
import play.api.mvc._
import service.NoteService

/**
  * This controller creates an `Action` to handle HTTP requests to the
  * application's home page.
  */
@Singleton
class HomeController @Inject()(cc: ControllerComponents, db: Database) extends AbstractController(cc) {

  private val contextList: List[ContextOption] = Score.availableContext.map(ContextOption(_, false))

  def index() = Action { implicit request: Request[AnyContent] =>
    Ok(views.html.index("", contextList.map(c => ContextOption(c.value, c.value == "all")), "", ""))
  }

  def search(query: String, context: String) = Action { implicit request: Request[AnyContent] =>
    val tokens = query.split(StringUtil.whiteSpaceSegmenter)
      .map(_.trim)
      .filter(_.length > 0)
      .distinct
      .toList
    val (category, body) = NoteService.search(tokens, Option(context))
    Ok(views.html.index(query, contextList.map(c => ContextOption(c.value, c.value == context)), category, body))
  }

  def getById(id: String) = Action { implicit request: Request[AnyContent] =>
    val (category, body) = NoteService.searchById(id)
    Ok(views.html.note("", List(), category, body))
  }

  def test = TODO
}

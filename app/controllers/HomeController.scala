package controllers

import javax.inject._

import play.api.db._
import play.api.mvc._

/**
  * This controller creates an `Action` to handle HTTP requests to the
  * application's home page.
  */
@Singleton
class HomeController @Inject()(cc: ControllerComponents, db: Database) extends AbstractController(cc) {

  def index() = Action { implicit request: Request[AnyContent] =>
    Ok(views.html.index(""))
  }

  def search(query: String) = Action { request: Request[AnyContent] =>
//    Ok("query = " + query)
    Ok(views.html.index(query))
  }

  def test = TODO
}

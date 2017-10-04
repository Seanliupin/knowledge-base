package controllers

import javax.inject._

import play.api.db._
import play.api.mvc._
import play.api.libs.json.Json

/**
  * This controller creates an `Action` to handle HTTP requests to the
  * application's home page.
  */
@Singleton
class HomeController @Inject()(cc: ControllerComponents, db: Database) extends AbstractController(cc) {

  /**
    * Create an Action to render an HTML page.
    *
    * The configuration in the `routes` file means that this method
    * will be called when the application receives a `GET` request with
    * a path of `/`.
    */
  def index() = Action { implicit request: Request[AnyContent] =>

    var result = "DB User:"
    val conn = db.getConnection()
    try {
      val rs = conn.createStatement().executeQuery("SELECT USER()")
      println("look " + rs)
      while (rs.next()) {
        result += rs.getString(1)
      }
    } finally {
      conn.close()
    }


    Ok(views.html.index("re = < " + result + " >"))
  }

  def ticketsAvailable = Action { request =>
    val availableTickets = 1000
    val result = Json.obj(
      "result" -> "ok",
      "ticketQuantity" -> availableTickets)
    Ok(result)
  }
}

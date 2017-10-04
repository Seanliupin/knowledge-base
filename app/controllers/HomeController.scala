package controllers

import javax.inject._

import com.dotgoing.prac.QueryString
import play.api.data.Form
import play.api.data.Forms._
import play.api.db._
import play.api.libs.json.Json
import play.api.mvc._

/**
  * This controller creates an `Action` to handle HTTP requests to the
  * application's home page.
  */
@Singleton
class HomeController @Inject()(cc: ControllerComponents, db: Database) extends AbstractController(cc) {


  def index() = Action { implicit request: Request[AnyContent] =>

    //    var result = "DB User:"
    //    val conn = db.getConnection()
    //    try {
    //      val rs = conn.createStatement().executeQuery("SELECT USER()")
    //      println("look " + rs)
    //      while (rs.next()) {
    //        result += rs.getString(1)
    //      }
    //    } finally {
    //      conn.close()
    //    }


    Ok(views.html.index(""))
  }

  def ticketsAvailable = Action { request: Request[AnyContent] =>
    val availableTickets = 1000
    val result = Json.obj(
      "result" -> "ok",
      "ticketQuantity" -> availableTickets)
    Ok(result)
  }

  def search = Action { request: Request[AnyContent] =>

    Ok(views.html.index(""))
  }

  def test = Action { request: Request[AnyContent] =>

    Ok(views.html.test(""))
  }
}

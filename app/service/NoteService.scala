package service

import java.util.concurrent.TimeUnit

import model.note.NoteBook

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.FiniteDuration
import scala.concurrent.{Await, Future}

/**
  * Author: Sean
  * Date: 4/10/2017
  * Time: 10:31 PM
  */
object NoteService {
  def search(tokens: List[String], context: Option[String]): (String, String) = {
    val body = new StringBuilder
    val category = new StringBuilder

    val pieces = Future.sequence(NoteBook.getPiece.map {
      piece =>
        Future {
          (piece.search(tokens, context), piece)
        }
    })

    Await.result(pieces, FiniteDuration(10, TimeUnit.SECONDS))
      .filter(_._1 != None).map {
      case (Some(x), y) => (x, y)
    }.distinct
      .sortWith((x, y) => x._1.score > y._1.score)
      .foreach(hitPair => {
        hitPair._2.urlRef match {
          case Some(ref) => category.append(ref)
          case None =>
        }
        body.append(hitPair._1.hit)
        //body.append(hitPair._1.hit + " score = " + hitPair._1.score)
      })

    (category.toString(), body.toString())
  }
}

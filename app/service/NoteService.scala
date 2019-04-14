package service

import java.util.concurrent.TimeUnit

import model.note.{HitScore, Note, NoteRepository}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.FiniteDuration
import scala.concurrent.{Await, Future}

/**
  * Author: Sean
  * Date: 4/10/2017
  * Time: 10:31 PM
  */
object NoteService {

  def parseTail(tokens: List[String], list: List[List[String]]): List[List[String]] = {
    val pre = tokens.takeWhile(_ != "&&")
    val rawTail = tokens.dropWhile(_ != "&&")
    val appendHead = list ++ List(pre)

    rawTail.length match {
      case 0 => appendHead
      case 1 => appendHead ++ List(List(rawTail.head))
      case _ => parseTail(rawTail.tail, appendHead)
    }
  }

  def search(tokens: List[String], context: Option[String]): (String, String) = {
    val conditions = parseTail(tokens, List()).filter(_.nonEmpty)
    if (conditions.isEmpty) {
      return ("", "")
    }

    val baseTargets = searchTarget(conditions.head, context, NoteRepository.getNotes)

    conditions.tail.foreach(out => {
      searchTarget(out, context, baseTargets.map(_.note))
    })

    val pieces = conditions.tail.foldLeft(baseTargets)((target, out) => {
      searchTarget(out, context, target.map(_.note))
    })

    val body = new StringBuilder
    val category = new StringBuilder

    pieces.sortWith((x, y) => x.score > y.score)
      .foreach(hitPair => {
        hitPair.note.urlRef
        hitPair.note.urlRef match {
          case Some(ref) => category.append(ref)
          case None =>
        }
        body.append(hitPair.hit)
      })

    (category.toString(), body.toString())
  }

  private def searchTarget(tokens: List[String], context: Option[String], notes: List[Note]): List[HitScore] = {
    val targetNotes = Future.sequence(notes.map {
      note =>
        Future {
          (note.search(tokens, context), note)
        }
    })

    val firstRound = Await.result(targetNotes, FiniteDuration(10, TimeUnit.SECONDS))
    firstRound.filter(_._1.isDefined).map(_._1.get).distinct
  }


  def searchById(id: String): (String, String) = {
    val body = new StringBuilder
    val category = new StringBuilder

    val pieces = Future.sequence(NoteRepository.getNotes.map {
      piece =>
        Future {
          (piece.searchById(id), piece)
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
      })

    (category.toString(), body.toString())
  }
}

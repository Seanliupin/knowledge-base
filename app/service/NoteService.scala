package service

import java.util.Observable

import helper.WatchDir
import model.note.{NoteBook, Piece}

/**
  * Author: Sean
  * Date: 4/10/2017
  * Time: 10:31 PM
  */
object NoteService {
  private var pieces: List[Piece] = List()
  private var inWatch = false;

  private def getPiece: List[Piece] = {
    if (!inWatch) {
      pieces = NoteBook.notes.flatMap(note => note.pieces)
      inWatch = true
//      WatchDir.watch(NoteBook.root, true, (_: Observable, notice: Any) => {
//        print(notice)
//        pieces = NoteBook.notes.flatMap(note => note.pieces)
//      })

    }
    pieces
  }

  def search(tokens: List[String], context: Option[String]): String = {
    val all = new StringBuilder

    val allPiece = getPiece

    val allHits = for {
      piece <- allPiece
      hit <- piece.search(tokens, context) //filter out None
    } yield hit

    allHits.sortWith((x, y) => x.score > y.score)
      //.foreach(hit => all.append(hit.hit + " score = " + hit.score))
      .foreach(hit => all.append(hit.hit))

    all.toString()
  }
}

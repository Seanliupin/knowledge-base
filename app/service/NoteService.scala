package service

import model.note.NoteBook

/**
  * Author: Sean
  * Date: 4/10/2017
  * Time: 10:31 PM
  */
object NoteService {
  def search(tokens: List[String], context: Option[String]): String = {
    val bodyString = new StringBuilder
    val cateString = new StringBuilder

    val pieces = NoteBook.getPiece

    val allHits = for {
      piece <- pieces
      hit <- piece.search(tokens, context) //filter out None
    } yield (hit, piece)

    allHits.sortWith((x, y) => x._1.score > x._1.score)
      //.foreach(hit => all.append(hit.hit + " score = " + hit.score))
      .foreach(hitPair => {
      cateString.append(hitPair._2.title) // 在这里构建目录
      bodyString.append(hitPair._1.hit)
    })

    bodyString.toString()
  }
}

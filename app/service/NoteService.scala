package service

import model.html.Node
import model.note.NoteBook

/**
  * Author: Sean
  * Date: 4/10/2017
  * Time: 10:31 PM
  */
object NoteService {
  def search(tokens: List[String], context: Option[String]): (String, String) = {
    val body = new StringBuilder
    val category = new StringBuilder

    val pieces = NoteBook.getPiece

    val allHits = for {
      piece <- pieces
      hit <- piece.search(tokens, context) //filter out None
    } yield (hit, piece)

    allHits.sortWith((x, y) => x._1.score > y._1.score)
      .foreach(hitPair => {
        hitPair._2.title match {
          case Some(innerTitle) => category.append(
            Node(Some("a"), innerTitle.line)
              .className("category-item")
              .addProperty("href", s"#${innerTitle.hrefId.getOrElse("")}")
              .setOuterNode(Node(Some("li"), "")))
          case None =>
        }
        body.append(hitPair._1.hit)
        //body.append(hitPair._1.hit + " score = " + hitPair._1.score)
      })

    (category.toString(), body.toString())
  }
}

package model.note

import helper.StringUtil
import resource.managed

/**
  * Author: Sean
  * Date: 4/10/2017
  * Time: 10:48 PM
  * 这个类表示一篇笔记
  */
case class Note(fileName: String) extends KnowledgeBase {

  /**
    * parse piece of information
    **/
  def getPiece: List[Piece] = {
    var pieces: List[Piece] = List()
    var piece = Piece(None, None)
    var codeBase = Code(None)

    for (source <- managed(scala.io.Source.fromFile(fileName))) {
      for (line <- source.getLines) {
        line match {
          case Extractor.codeFooterExtractor() => {
            if (codeBase.isValidCode) {
              piece.addLine(codeBase)
              codeBase = Code(None)
            } else {
              codeBase = Code(Some(""))
            }
          }
          case Extractor.codeHeaderExtractor(language) => {
            if (codeBase.hasCode) {
              piece.addLine(codeBase)
            }
            codeBase = Code(Some(language))
          }
          case code if codeBase.isValidCode => codeBase.addCode(code)

          case Extractor.titleExtractor(title) => {
            if (piece.isValid) {
              pieces = pieces ++ List(piece)
            }
            piece = Piece(Some(title), Option(fileName))
          }
          case Extractor.tagsExtractor(tags) if piece.isValid =>
            tags.split(StringUtil.whiteSpaceSegmenter)
              .toList.map(_.trim)
              .filter(_.length > 0)
              .foreach(keyWord => piece.addLine(KeyWord(keyWord)))
          case Extractor.keysExtractor(keys) if piece.isValid =>
            keys.split(StringUtil.whiteSpaceSegmenter)
              .toList.map(_.trim)
              .filter(_.length > 0)
              .foreach(keyWord => piece.addLine(KeyWord(keyWord)))
          case Extractor.timeExtractor(time) if piece.isValid => piece.setTime(time)
          case Extractor.WebExtractor(title, url, comment) if piece.isValid => piece.addLine(Web(title, url, comment))
          case Extractor.WebItemExtractor(title, url, comment) if piece.isValid => piece.addLine(Web(title, url, comment))
          case Extractor.bookExtractor(title, url, comment) if piece.isValid => piece.addLine(Book(title, url, comment))
          case Extractor.commentExtractor(comment) if piece.isValid => piece.addLine(Comment(comment))
          case Extractor.timeExtractor(time) if piece.isValid => piece.setTime(time)
          case Extractor.subTitleExtractor(subTitle) if piece.isValid => piece.addLine(SubTitle(subTitle))
          case _ if piece.isValid => piece.addLine(Line(line))
          case _ =>
        }
      }
    }

    if (piece.isValid) {
      pieces = pieces ++ List(piece)
    }

    pieces
  }

  override def search(tokens: List[String], context: Option[String]): List[HitScore] = {
    getPiece.flatMap(piece => {
      piece.search(tokens, context)
    }).filter(_.score > 0)
  }
}

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
  def pieces: List[Piece] = {
    var pieces: List[Piece] = List()
    var piece = Piece(None, None)
    var codeBlock = Code(None, None)
    var commentBlock = Comment(None, None)

    for {
      source <- managed(scala.io.Source.fromFile(fileName))
      line <- source.getLines
    } {
      line match {
        case Extractor.codeFooterExtractor() => {
          if (codeBlock.isValid) {
            piece.addLine(codeBlock)
            codeBlock = Code(None, None)
          } else {
            codeBlock = Code(Some(""), Some(""))
          }
        }
        case Extractor.commentFooterExtractor() => {
          if (commentBlock.isValid) {
            piece.addLine(commentBlock)
            commentBlock = Comment(None, None)
          } else {
            commentBlock = Comment(Some(""), Some(""))
          }
        }
        case Extractor.codeHeaderExtractor(lan, title) => {
          if (!codeBlock.isEmpty) {
            piece.addLine(codeBlock)
          }
          codeBlock = Code(Some(lan), Some(title.trim))
        }
        case Extractor.commentHeaderExtractor(ctype, title) => {
          if (!commentBlock.isEmpty) {
            piece.addLine(commentBlock)
          }

          commentBlock = Comment(Some(ctype), Some(title))
        }
        case code if codeBlock.isValid => codeBlock.addLine(code)
        case comment if commentBlock.isValid => commentBlock.addLine(comment)

        case Extractor.titleExtractor(title) => {
          if (piece.isValid) {
            pieces = piece +: pieces
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
        case Extractor.timeExtractor(time) if piece.isValid => piece.setTime(time)
        case Extractor.subTitleExtractor(subTitle) if piece.isValid => piece.addLine(SubTitle(subTitle))
        case Extractor.typedTipExtractor(tipType, tip) if piece.isValid => piece.addLine(Tip(tip, Some(tipType)))
        case Extractor.typeLessTipExtractor(tip) if piece.isValid => piece.addLine(Tip(tip, None))
        case _ if piece.isValid => piece.addLine(Line(line))
        case _ =>
      }
    }

    if (piece.isValid) {
      pieces = piece +: pieces
    }

    pieces
  }

  override def search(tokens: List[String], context: Option[String]): List[HitScore] = {
    for {
      piece <- pieces
      hit <- piece.search(tokens, context)
    } yield hit
  }
}

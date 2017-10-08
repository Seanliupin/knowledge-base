package model.note

import helper.StringUtil
import resource.managed

/**
  * Author: Sean
  * Date: 4/10/2017
  * Time: 10:48 PM
  * 这个类表示一篇笔记
  */
case class Note(fileName: String) {

  /**
    * parse piece of information
    **/
  def pieces: List[Piece] = {
    var pieces: List[Piece] = List()
    var piece = Piece(None, None)
    var codeBlock = Code(None, None)
    var commentBlock = Comment(None, None)
    var globalTags: List[KeyWord] = List()

    def addPiece(piece: Piece): List[Piece] = {
      globalTags.foreach(piece.addLine)
      pieces = piece +: pieces
      pieces
    }

    for {
      source <- managed(scala.io.Source.fromFile(fileName))
      line <- source.getLines
    } {
      line match {
        case Extractor.globalTagsExtractor(tags) if !piece.isValid => {
          globalTags = tags.split(StringUtil.whiteSpaceSegmenter)
            .toList.map(KeyWord(_))
        }

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
            addPiece(piece)
          }
          piece = Piece(Some(title), Option(fileName))
        }
        case Extractor.tagsExtractor(tags) if piece.isValid =>
          tags.split(StringUtil.whiteSpaceSegmenter)
            .map(KeyWord(_))
            .foreach(piece.addLine)
        case Extractor.keysExtractor(keys) if piece.isValid =>
          keys.split(StringUtil.whiteSpaceSegmenter)
            .map(KeyWord(_))
            .foreach(piece.addLine)
        case Extractor.timeExtractor(time) if piece.isValid => piece.setTime(Time(time))
        case Extractor.WebExtractor(title, url, comment) if piece.isValid => piece.addLine(Web(title, url, comment))
        case Extractor.WebItemExtractor(title, url, comment) if piece.isValid => piece.addLine(Web(title, url, comment))
        case Extractor.bookExtractor(title, url, comment) if piece.isValid => piece.addLine(Book(title, url, comment))
        case Extractor.subTitleExtractor(subTitle) if piece.isValid => piece.addLine(SubTitle(subTitle))
        case Extractor.typedTipExtractor(tipType, tip) if piece.isValid => piece.addLine(Tip(tip, Some(tipType)))
        case Extractor.typeLessTipExtractor(tip) if piece.isValid => piece.addLine(Tip(tip, None))
        case _ if piece.isValid => piece.addLine(Line(line))
        case _ =>
      }
    }

    if (piece.isValid) {
      addPiece(piece)
    }

    pieces
  }
}

package model.note

import helper.StringUtil
import resource.managed

/**
  * Author: Sean
  * Date: 4/10/2017
  * Time: 10:48 PM
  */
case class NoteFile(fileName: String) {

  /**
    * parse piece of information
    **/
  def pieces: List[Note] = {
    var pieces: List[Note] = List()
    var piece = Note(None, None)
    var codeBlock = Code(None, None)
    var commentBlock = Memo(None, None)
    var globalTags: List[KeyWord] = List()
    var globalTitle: Option[String] = None
    var globalYear: Option[String] = None

    fileName match {
      case Extractor.titleYearExtractor(_, title, year, _) => {
        globalTitle = Some(title)
        globalYear = Some(year)
      }
      case _ =>
    }

    def getHashCode(str: String): String = {
      s"${str.hashCode}"
    }

    def addPiece(piece: Note): List[Note] = {
      globalTags.foreach(piece.addLine)
      var fixPiece = piece

      if (piece.hasNotTime) {
        for {
          titleFromFile <- globalTitle
          year <- globalYear
          oldTitle <- piece.title
        } {
          oldTitle.line match {
            case Extractor.dayMonthExtractor(month, day) => {
              fixPiece = Note(Some(Title(titleFromFile, Some(getHashCode(s"$fileName$oldTitle")))), piece.fileName)
              fixPiece.setTime(Time(s"$year/$month/$day"))
              fixPiece.setLines(piece.getLines)
            }
            case _ =>
          }
        }
      }

      pieces = fixPiece +: pieces
      pieces
    }

    for {
      source <- managed(scala.io.Source.fromFile(fileName, "UTF-8"))
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
            commentBlock = Memo(None, None)
          } else {
            commentBlock = Memo(Some(""), Some(""))
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

          commentBlock = Memo(Some(ctype), Some(title))
        }
        case code if codeBlock.isValid => codeBlock.addLine(code)
        case comment if commentBlock.isValid => commentBlock.addLine(comment)

        case Extractor.titleExtractor(title) => {
          if (piece.isValid) {
            addPiece(piece)
          }
          piece = Note(Some(Title(title, Some(getHashCode(s"$fileName$title")))), Option(fileName))
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
        case Extractor.idExtractor(id) if piece.isValid => piece.addLine(Id(id.trim))
        case Extractor.WebExtractor(title, url, comment) if piece.isValid => piece.addLine(Web(title, url, comment))
        case Extractor.WebItemExtractor(title, url, comment) if piece.isValid => piece.addLine(Web(title, url, comment))
        case Extractor.bookExtractor(title, url, comment) if piece.isValid => piece.addLine(Book(title, url, comment))
        case Extractor.subTitleExtractor(subTitle) if piece.isValid => piece.addLine(SubTitle(subTitle))
        case Extractor.scriptExtractor(src, des) if piece.isValid => piece.addLine(Script(src, des))
        case Extractor.typedTipExtractor(tipType, tip) if piece.isValid => {
          if (tipType.trim.length == 0) {
            piece.addLine(Tip(tip, None))
          } else {
            piece.addLine(Tip(tip, Some(tipType)))
          }
        }
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

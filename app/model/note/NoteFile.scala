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
  def notes: List[Note] = {
    var pieces: List[Note] = List()
    var note = Note(None, None)
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

    def addNote(note: Note): List[Note] = {
      globalTags.foreach(note.addLine)
      var fixNote = note

      if (note.hasNotTime) {
        for {
          titleFromFile <- globalTitle
          year <- globalYear
          oldTitle <- note.title
        } {
          oldTitle.line match {
            case Extractor.dayMonthExtractor(month, day) => {
              fixNote = Note(Some(Title(titleFromFile, Some(getHashCode(s"$fileName$oldTitle")))), note.fileName)
              fixNote.setTime(Time(s"$year/$month/$day"))
              fixNote.setLines(note.getLines)
            }
            case _ =>
          }
        }
      }

      pieces = fixNote +: pieces
      pieces
    }

    for {
      source <- managed(scala.io.Source.fromFile(fileName, "UTF-8"))
      line <- source.getLines
    } {
      line match {
        case Extractor.globalTagsExtractor(tags) if !note.isValid => {
          globalTags = tags.split(StringUtil.whiteSpaceSegmenter)
            .toList.map(KeyWord(_))
        }

        case Extractor.codeFooterExtractor() => {
          if (codeBlock.isValid) {
            note.addLine(codeBlock)
            codeBlock = Code(None, None)
          } else {
            codeBlock = Code(Some(""), Some(""))
          }
        }
        case Extractor.commentFooterExtractor() => {
          if (commentBlock.isValid) {
            note.addLine(commentBlock)
            commentBlock = Memo(None, None)
          } else {
            commentBlock = Memo(Some(""), Some(""))
          }
        }
        case Extractor.codeHeaderExtractor(lan, title) => {
          if (!codeBlock.isEmpty) {
            note.addLine(codeBlock)
          }
          codeBlock = Code(Some(lan), Some(title.trim))
        }
        case Extractor.commentHeaderExtractor(ctype, title) => {
          if (!commentBlock.isEmpty) {
            note.addLine(commentBlock)
          }

          commentBlock = Memo(Some(ctype), Some(title))
        }
        case code if codeBlock.isValid => codeBlock.addLine(code)
        case comment if commentBlock.isValid => commentBlock.addLine(comment)

        case Extractor.titleExtractor(title) => {
          if (note.isValid) {
            addNote(note)
          }
          note = Note(Some(Title(title, Some(getHashCode(s"$fileName$title")))), Option(fileName))
        }
        case Extractor.tagsExtractor(tags) if note.isValid =>
          tags.split(StringUtil.whiteSpaceSegmenter)
            .map(KeyWord(_))
            .foreach(note.addLine)
        case Extractor.keysExtractor(keys) if note.isValid =>
          keys.split(StringUtil.whiteSpaceSegmenter)
            .map(KeyWord(_))
            .foreach(note.addLine)
        case Extractor.timeExtractor(time) if note.isValid => note.setTime(Time(time))
        case Extractor.idExtractor(id) if note.isValid => note.addLine(Id(id.trim))
        case Extractor.WebExtractor(title, url, comment) if note.isValid => note.addLine(Web(title, url, comment))
        case Extractor.WebItemExtractor(title, url, comment) if note.isValid => note.addLine(Web(title, url, comment))
        case Extractor.bookExtractor(title, url, comment) if note.isValid => note.addLine(Book(title, url, comment))
        case Extractor.subTitleExtractor(subTitle) if note.isValid => note.addLine(SubTitle(subTitle))
        case Extractor.scriptExtractor(src, des) if note.isValid => note.addLine(Script(src, des))
        case Extractor.typedTipExtractor(tipType, tip) if note.isValid => {
          if (tipType.trim.length == 0) {
            note.addLine(Tip(tip, None))
          } else {
            note.addLine(Tip(tip, Some(tipType)))
          }
        }
        case Extractor.typeLessTipExtractor(tip) if note.isValid => note.addLine(Tip(tip, None))
        case _ if note.isValid => note.addLine(Line(line))
        case _ =>
      }
    }

    if (note.isValid) {
      addNote(note)
    }

    pieces
  }
}

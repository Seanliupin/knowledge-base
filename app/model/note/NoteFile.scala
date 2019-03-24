package model.note

import helper.StringUtil
import resource.managed

/**
  * Author: Sean
  * Date: 4/10/2017
  * Time: 10:48 PM
  */
case class NoteFile(fileName: String) {

  def linesToNotes(lines: List[String], fN: String, globalTitle: Option[String], globalYear: Option[String]): List[Note] = {
    var pieces: List[Note] = List()
    var note = Note(None, None)
    var codeBlock = Code(None, None)
    var memoBlock = Memo(None, None)
    var globalTags: List[KeyWord] = List()

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
              fixNote = Note(Some(Title(titleFromFile, Some(getHashCode(s"hash-$fN$oldTitle")))), note.fileName)
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

    lines.foreach {
      case Extractor.globalTagsExtractor(tags) if !note.isValid => {
        globalTags = tags.split(StringUtil.whiteSpaceSegmenter)
          .toList.map(KeyWord)
      }

      case Extractor.codeFooterExtractor() => {
        if (codeBlock.isValid) {
          note.addLine(codeBlock)
          codeBlock = Code(None, None)
        } else {
          codeBlock = Code(Some(""), Some(""))
        }
      }
      case Extractor.memoFooterExtractor() => {
        if (memoBlock.isValid) {
          note.addLine(memoBlock)
          memoBlock = Memo(None, None)
        } else {
          memoBlock = Memo(Some(""), Some(""))
        }
      }
      case Extractor.codeHeaderExtractor(lan, title) => {
        if (!codeBlock.isEmpty) {
          note.addLine(codeBlock)
        }
        codeBlock = Code(Some(lan), Some(title.trim))
      }
      case Extractor.memoHeaderExtractor(ctype, title) => {
        if (!memoBlock.isEmpty) {
          note.addLine(memoBlock)
        }

        memoBlock = Memo(Some(ctype), Some(title))
      }
      case code if codeBlock.isValid => codeBlock.addLine(code)
      case comment if memoBlock.isValid => memoBlock.addLine(comment)

      case Extractor.titleExtractor(title) => {
        if (note.isValid) {
          addNote(note)
        }
        note = Note(Some(Title(title, Some(getHashCode(s"$fileName$title")))), Option(fileName))
      }
      case Extractor.tagsExtractor(tags) if note.isValid =>
        tags.split(StringUtil.whiteSpaceSegmenter)
          .map(KeyWord)
          .foreach(note.addLine)
      case Extractor.keysExtractor(keys) if note.isValid =>
        keys.split(StringUtil.whiteSpaceSegmenter)
          .map(KeyWord)
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
      case line if note.isValid => note.addLine(Line(line))
      case _ =>
    }

    if (note.isValid) {
      addNote(note)
    }
    pieces
  }

  /**
    * parse piece of information
    **/
  def notes: List[Note] = {
    if (fileName.trim.isEmpty) {
      return List()
    }

    var globalTitle: Option[String] = None
    var globalYear: Option[String] = None

    fileName match {
      case Extractor.titleYearExtractor(_, title, year, _) => {
        globalTitle = Some(title)
        globalYear = Some(year)
      }
      case _ =>
    }

    for {source <- managed(scala.io.Source.fromFile(fileName, "UTF-8"))} {
      return linesToNotes(source.getLines().toList, fileName, globalTitle, globalYear)
    }
    List()
  }
}

package model.note

import java.nio.charset.CodingErrorAction

import helper.StringUtil
import resource.managed

import scala.io.Codec
import scala.reflect.io.File

/**
  * Author: Sean
  * Date: 4/10/2017
  * Time: 10:48 PM
  */
case class NoteFile(noteFilePath: String) {

  def linesToNotes(lines: List[String], filePath: String, globalTitle: Option[String], globalYear: Option[String], firstTitle: Option[Title]): List[Note] = {
    var pieces: List[Note] = List()
    var note = Note(firstTitle, None)
    var codeBlock = Code(None, None)
    var memoBlock = Memo(None, None)
    var globalTags: List[Tag] = List()

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
          oldTitle.title.getOrElse("") match {
            case Extractor.dayMonthExtractor(month, day) => {
              fixNote = Note(Some(Title(Some(titleFromFile), Some(getHashCode(s"hash-$filePath$oldTitle")))), note.fileName)
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
          .toList.map(Tag)
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
        note = Note(Some(Title(Some(title), Some(getHashCode(s"$filePath$title")))), Option(filePath))
        note.addLine(FileName(File(filePath).name))
      }
      case Extractor.tagsExtractor(tags) if note.isValid =>
        tags.split(StringUtil.whiteSpaceSegmenter)
          .map(Tag)
          .foreach(note.addLine)
      case Extractor.keysExtractor(keys) if note.isValid =>
        keys.split(StringUtil.whiteSpaceSegmenter)
          .map(Tag)
          .foreach(note.addLine)
      case Extractor.timeExtractor(time) if note.isValid => note.setTime(Time(time))
      case Extractor.idExtractor(id) if note.isValid => note.addLine(Id(id.trim))
      case Extractor.WebExtractor(title, url, comment) if note.isValid => note.addLine(Web(title, url, comment))
      case Extractor.WebItemExtractor(title, url, comment) if note.isValid => note.addLine(Web(title, url, comment))
      case Extractor.bookExtractor(title, url, comment) if note.isValid => note.addLine(Book(title, url, comment))
      case Extractor.subTitleExtractor(subTitle) if note.isValid => note.addLine(SubTitle(subTitle))
      case Extractor.frameExtractor(attribute, des) if note.isValid => {
        attribute match {
          case Extractor.srcExtractor(_, src, _) => note.addLine(Frame(attribute, des, Some(src)))
          case _ => note.addLine(Frame(attribute, des))
        }
      }
      case Extractor.asciinemaPlayerExtractor(attribute, des) if note.isValid => {
        attribute match {
          case Extractor.srcExtractor(_, src, _) => note.addLine(AsciinemaPlayer(attribute, des, Some(src)))
          case _ => note.addLine(Frame(attribute, des))
        }
      }
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
    pieces.reverse
  }

  /**
    * parse piece of information
    **/
  def notes: List[Note] = {
    if (noteFilePath.trim.isEmpty) {
      return List()
    }

    var globalTitle: Option[String] = None
    var globalYear: Option[String] = None

    noteFilePath match {
      case Extractor.titleYearExtractor(_, title, year, _) => {
        globalTitle = Some(title)
        globalYear = Some(year)
      }
      case _ =>
    }

    implicit val codec = Codec.UTF8
    codec.onMalformedInput(CodingErrorAction.IGNORE)
    codec.onUnmappableCharacter(CodingErrorAction.IGNORE)

    for {source <- managed(scala.io.Source.fromFile(noteFilePath))} {
      return linesToNotes(source.getLines().toList, noteFilePath, globalTitle, globalYear, None)
    }
    List()
  }
}

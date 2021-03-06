package model.note

import java.io.File
import java.util.Observable

import helper.WatchDir

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

/**
  * Author: Sean
  * Date: 4/10/2017
  * Time: 10:46 PM
  */
object NoteRepository {
  val root = "/home/note/"
  //   val root = "/Users/seanliu/Note/ztodo"  // for test

  private var allNotes: List[Note] = List()
  private var inWatch = false;

  def getNotes: List[Note] = {

    if (!inWatch) {
      Future {
        inWatch = true
        WatchDir.watch(NoteRepository.root, true, (_: Observable, notice: Any) => {
          allNotes = NoteRepository.notes.flatMap(note => note.notes)
        })
      }

      allNotes = NoteRepository.notes.flatMap(note => note.notes)
    }

    allNotes
  }

  private def notes: List[NoteFile] = {
    files.map(file => {
      NoteFile(file.getAbsolutePath)
    })
  }

  private def files: List[File] = {
    @scala.annotation.tailrec
    def sc(acc: List[File], files: List[File]): List[File] = {
      files match {
        case Nil => acc
        case x :: xs => {
          val toIgnore = x.getName.startsWith(".")
          toIgnore match {
            case true => sc(acc, xs)
            case false => {
              x.isDirectory match {
                case false => sc(x :: acc, xs)
                case true => sc(acc, xs ::: x.listFiles.toList)
              }
            }
          }
        }
      }
    }

    val files = sc(List(), List(new File(root)))
    files.filter(f => f.getName.endsWith(".md"))
  }
}

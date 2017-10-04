package service

import java.io.File

import resource.managed

/**
  * Author: Sean
  * Date: 4/10/2017
  * Time: 10:31 PM
  */
object NoteService {
  private val root = "/Users/seanliu/Note/"

  def search(tokens: List[String]): String = {
    val all = new StringBuilder
    files.foreach(file => {
      val re = searchInFile(file.getAbsolutePath, tokens)
      all.append(re)
    })
    all.toString()
  }

  private def renderHit(text: String, token: String): String = {
    text.replaceAll(token, "<strong class=\"text-danger\">" + token + "</strong>")
  }

  private def searchInFile(fileName: String, tokens: List[String]) = {
    val s = new StringBuilder
    for (source <- managed(scala.io.Source.fromFile(fileName))) {
      for (line <- source.getLines) {
        val contain = tokens.exists(token => {
          line.contains(token)
        })

        if (contain) {
          var renderedLine = line
          tokens.foreach(token => {
            renderedLine = renderHit(renderedLine, token)
          })
          s.append(renderedLine)
          s.append("<br/> <br/>")
        }
      }
    }

    s.toString()
  }

  private def files(): List[File] = {
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

    sc(List(), List(new File(root)))
  }

}

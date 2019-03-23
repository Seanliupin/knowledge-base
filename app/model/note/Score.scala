package model.note

/**
  * Author: Sean
  * Date: 5/10/2017
  * Time: 3:47 PM
  */
object Score {

  /**
    * keyword symbol weight
    **/
  val contexts: List[(String, Symbol, Int)] = {
    List(
      ("all", 'All, 0),
      ("title", 'Title, 350),
      ("keyword", 'KeyWord, 400),
      ("subtitle", 'SubTitle, 250),
      ("memo", 'Memo, 150),
      ("web", 'Web, 80),
      ("book", 'Book, 80),
      ("code", 'Code, 50),
      ("url", 'Url, 100),
      ("line", 'Line, 100),
      ("tip", 'Tip, 140),
      ("script", 'Script, 120),
      ("id", 'id, 10),
      ("time", 'Time, 50)
    )
  }

  def availableContext: List[String] = {
    val filterOut = List("book", "web", "time", "subtitle")
    val re = contexts.map(_._1).filter(item => !filterOut.exists(item == _))
    re
  }

  def getScore(symbol: Symbol): Int = {
    contexts.foreach(context => {
      if (context._2 == symbol) {
        return context._3
      }
    })
    return 0
  }

  def keyWordToSymbol(keyword: Option[String]): Option[Symbol] = {
    keyword match {
      case Some(key) => {
        contexts.foreach {
          case (searchKey, symbol, _) if searchKey == key => return Some(symbol)
          case _ =>
        }
      }
      case None =>
    }
    None
  }

  /**
    * (wordMatch, caseMatch)
    **/
  def getMatchScore(m: (Boolean, Boolean)): Int = {
    m match {
      case (true, true) => 10
      case (true, false) => 8
      case (false, true) => 3
      case (false, false) => 1
    }
  }

}

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
  private val searchableContexts: List[(String, Symbol, Int)] = {
    List(
      ("all", 'All, 0),
      ("title", 'Title, 350),
      ("tag", 'Tag, 400),
      ("memo", 'Memo, 150),
      ("code", 'Code, 50),
      ("url", 'Url, 100),
      ("line", 'Line, 100),
      ("frame", 'Frame, 100),
      ("tip", 'Tip, 140)
    )
  }

  private val notSearchableContexts: List[(String, Symbol, Int)] = {
    List(
      ("subtitle", 'SubTitle, 250),
      ("memoTitle", 'MemoTitle, 200),
      ("codeTitle", 'CodeTitle, 200),
      ("web", 'Web, 80),
      ("book", 'Book, 80),
      ("time", 'Time, 50),
      ("id", 'id, 10),
      ("fileName", 'FileName, 50)
    )
  }

  def availableContext: List[String] = {
    searchableContexts.map(_._1)
  }

  def getScore(symbol: Symbol): Int = {
    (notSearchableContexts ++ notSearchableContexts).foreach(context => {
      if (context._2 == symbol) {
        return context._3
      }
    })
    10
  }

  def keyWordToSymbol(keyword: Option[String]): Option[Symbol] = {
    keyword match {
      case Some(key) => {
        searchableContexts.foreach {
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

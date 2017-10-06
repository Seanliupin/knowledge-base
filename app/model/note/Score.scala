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
      ("title", 'Title, 30),
      ("keyword", 'KeyWord, 30),
      ("subtitle", 'SubTitle, 25),
      ("comment", 'Comment, 20), //Comment 其实就是标记为note的Tip
      ("web", 'Web, 20),
      ("book", 'Book, 20),
      ("code", 'Code, 10),
      ("line", 'Line, 10),
      ("tip", 'Tip, 10),
      ("time", 'Time, 0)

    )
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
        contexts.foreach(context => {
          if (context._1 == key) {
            return Some(context._2)
          }
        })
        return None
      }
      case _ => None
    }
  }

}

package model.note

/**
  * Author: Sean
  * Date: 5/10/2017
  * Time: 3:47 PM
  */
object Score {
  var scoreTitle = 30
  var scoreBody = 10
  var scoreComment = 20
  var scoreTag = 30
  var scoreWeb = 20
  var scoreBook = 20


  def getScore(symbol: Symbol): Int = {
    symbol match {
      case 'Title => 30
      case 'KeyWord => 30
      case 'Comment => 20
      case 'SubTitle => 25
      case 'Web => 20
      case 'Book => 20
      case 'Code => 10
      case 'Line => 10
      case 'Time => 0
      case _ => 0
    }
  }

  def keyWordToSymbol(keyword: Option[String]): Option[Symbol] = {
    keyword match {
      case Some("title") => Some('Title)
      case Some("keyword") => Some('KeyWord)
      case Some("comment") => Some('Comment)
      case Some("subtitle") => Some('SubTitle)
      case Some("web") => Some('Web)
      case Some("book") => Some('Book)
      case Some("code") => Some('Code)
      case Some("body") => Some('Line)
      case Some("time") => Some('Time)
      case Some("all") => Some('All)
      case _ => None
    }

  }
}

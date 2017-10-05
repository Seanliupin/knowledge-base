package model.note

/**
  * Author: Sean
  * Date: 5/10/2017
  * Time: 10:08 AM
  */
case class Text(title: String, fileName: Option[String]) extends Piece(title, fileName) {

}

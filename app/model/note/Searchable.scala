package model.note

/**
  * Author: Sean
  * Date: 5/10/2017
  * Time: 10:02 AM
  */
trait Searchable {
  def search(tokens: List[String], context: Option[String]): List[Hit]
}

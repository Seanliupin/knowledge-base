package model.note

/**
  * Author: Sean
  * Date: 5/10/2017
  * Time: 7:26 PM
  */
trait Hitable {
  def hit(token: String): Boolean
}

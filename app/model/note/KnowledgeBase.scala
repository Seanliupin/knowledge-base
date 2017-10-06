package model.note

/**
  * Author: Sean
  * Date: 5/10/2017
  * Time: 10:02 AM
  * KnowledgeBase 其内包含了代码，超链接，评注等
  */
trait KnowledgeBase {
  def search(tokens: List[String], context: Option[String]): List[HitScore]
}

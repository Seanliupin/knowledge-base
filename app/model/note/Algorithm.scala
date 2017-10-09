package model.note

/**
  * Creator   Sean
  * Date      2017/10/10
  * Time      0:15
  */
object Algorithm {

  /**
    * List[(是否全匹配, 是否大小写匹配，单词位置)]
    * 命中文本的类型
    **/
  def computeScore(matchList: List[(Boolean, Boolean, Int)], targetType: Symbol): Int = {
    if (matchList.size == 0) return 0

    val list: List[(Boolean, Int, Int)] = matchList.map(x => (x._1, Score.getMatchScore(x._1, x._2), x._3))

    val baseScore = Score.getScore(targetType)
    val fullScore = list.map(x => (x._1, x._2 * baseScore, x._3))
    var totalScore = 0

    /**
      * 就像经济学里的效用递减一样，每增加一个同类的东西，其收益递减
      * (a,b,c,d ...)
      * sum = a/1 + b/1.5 + c/2 + d/2.5 ...
      **/

    def halfScoreIt(list: List[Int]): Int = {
      val s = list.zipWithIndex
        .map(x => (x._1, x._2 + 2))
        .map(x => x._1 / (x._2 * 0.5)).sum
      Math.floor(s).toInt
    }

    totalScore += halfScoreIt(fullScore.filter(_._1).map(_._2)) // List[(匹配分)]
    totalScore += halfScoreIt(fullScore.filter(!_._1).map(_._2))

    //todo: 还可以把命中词之间的距离加上去，这一项最高得分为一个完整命中单词的基本分
    totalScore
  }
}



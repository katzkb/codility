package lesson8

// https://app.codility.com/demo/results/trainingUER29T-B2T/
object Q2EquiLeader {
  def solution(a: Array[Int]): Int = {
    val size = a.length
    val aVector = a.toVector

    @scala.annotation.tailrec
    def getLeader(
      remain:        Vector[Int],
      cumulativeSum: Map[Int, Int] = Map.empty,
      stack:         Vector[Int] = Vector.empty[Int]
    ): (Map[Int, Int], Option[Int]) = {
      remain match {
        case head +: tail =>
          val newStack =
            if (stack.isEmpty) {
              Vector(head)
            } else if (head == stack.last) {
              stack :+ head
            } else {
              stack.init
            }
          val cum = cumulativeSum + (head -> (cumulativeSum.getOrElse(head, 0) + 1))
          getLeader(tail, cum, newStack)
        case _ => (cumulativeSum, stack.headOption)
      }
    }

    val (cumulativeSumMap, leaderOpt) = getLeader(aVector)

    @scala.annotation.tailrec
    def findMatch(
      remain:   Vector[Int],
      leader:   Int,
      rLeader:  Int = 0,
      lLeader:  Int = 0,
      matchNum: Int = 0,
      cnt:      Int = 1
    ): Int = {
      remain match {
        case head +: tail =>
          val left  = lLeader + (if (head == leader) 1  else 0)
          val right = rLeader + (if (head == leader) -1 else 0)
          val (lSize, rSize)  = (cnt / 2, (size - cnt) / 2)
          val newM = matchNum + (if (left > lSize && right > rSize) 1 else 0)
          findMatch(tail, leader, right, left, newM, cnt + 1)
        case _ => matchNum
      }
    }
    leaderOpt match {
      case Some(leader) if size > 1 => findMatch(aVector, leader, cumulativeSumMap(leader))
      case _                        => 0
    }
  }
}

package lesson8

object Q1Dominator {
  def solution(a: Array[Int]): Int = {
    val halfSize = a.length / 2

    @scala.annotation.tailrec
    def check(
      remain: Vector[Int],
      total:  Map[Int, Int] = Map.empty,
      keyMap: Map[Int, Int] = Map.empty,
      key:    Int = -1
    ): (Map[Int, Int], Map[Int, Int]) = {
      remain match {
        case head +: tail =>
          val newCnt    = total.getOrElse(head, 0) + 1
          val newTotal  = total + (head -> newCnt)
          val newKeyMap = keyMap + (head -> (key + 1))
          if (newCnt > halfSize) {
            (newTotal, newKeyMap)
          } else {
            check(tail, newTotal, newKeyMap, key + 1)
          }
        case _ => (total, keyMap)
      }
    }

    val (tMap, kMap) = check(a.toVector)
    tMap.collectFirst {
      case (k, v) if v > halfSize => kMap(k)
    }.getOrElse(-1)
  }
}

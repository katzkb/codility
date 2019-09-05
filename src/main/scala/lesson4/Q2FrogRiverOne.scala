package lesson4

// https://app.codility.com/demo/results/trainingGAH3SM-SBJ/
object Q2FrogRiverOne {
  def solution(x: Int, a: Array[Int]): Int = {
    // write your code in Scala 2.12
    @scala.annotation.tailrec
    def check(aRemain: Vector[Int], nSet: Set[Int] = Set.empty, cnt: Int = 0): Int =
      aRemain match {
        case head +: tail =>
          val newNSet = nSet + head
          if (newNSet.size == x) {
            cnt
          } else {
            check(tail, newNSet, cnt + 1)
          }
        case _ => -1
      }
    check(a.toVector)
  }
}

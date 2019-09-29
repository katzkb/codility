package lesson4

// https://app.codility.com/demo/results/trainingQ95YCG-H2B/
object Q4MissingInteger {
  def solution(a: Array[Int]): Int = {
    val aVector = a.toSet.toVector.filter(_ > 0).sorted
    @scala.annotation.tailrec
    def check(remain: Vector[Int], cnt: Int = 1): Int = {
      remain match {
        case head +: tail =>
          if (head != cnt) {
            cnt
          } else {
            check(tail, cnt + 1)
          }
        case _ => if (aVector.min == 1) cnt else 1
      }
    }
    if (aVector.isEmpty) 1 else check(aVector)
  }
}

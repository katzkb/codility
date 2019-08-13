package lesson2

// https://app.codility.com/demo/results/trainingW4ZTZQ-8ZV/
object Q1OddOccurrencesInArray {
  def solution(a: Array[Int]): Int = {
    // write your code in Scala 2.12
    val vec = a.sorted.toVector
    @scala.annotation.tailrec
    def f(remain: Vector[Int], cnt: Int): Int = {
      val lastIndex = remain.indexWhere(_ != cnt)
      if (lastIndex % 2 != 0) {
        cnt
      } else {
        f(remain.drop(lastIndex), remain(lastIndex))
      }
    }
    f(vec, vec.min)
  }
}

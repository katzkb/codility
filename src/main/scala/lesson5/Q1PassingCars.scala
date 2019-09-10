package lesson5

// https://app.codility.com/demo/results/training44HZ2T-QK8/
object Q1PassingCars {
  def solution(a: Array[Int]): Int = {
    @scala.annotation.tailrec
    def check(remain: Vector[Int], rate: Int = 0, cnt: Int = 0): Int = {
      remain match {
        case head +: remain =>
          if (head == 0) {
            check(remain, rate + 1, cnt)
          } else {
            check(remain, rate, cnt + rate)
          }
        case _ =>
          if (cnt > 1000000000 || cnt < -1000000000) -1 else cnt
      }
    }
    check(a.toVector)
  }
}

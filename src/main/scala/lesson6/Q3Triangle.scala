package lesson6

// https://app.codility.com/demo/results/trainingBAS68R-JZ5/
object Q3Triangle {
  def solution(a: Array[Int]): Int = {
    @scala.annotation.tailrec
    def check(remain: Vector[Long]): Int = {
      remain match {
        case head +: second +: third +: tail =>
          if (
            head + second > third &&
              second + third > head &&
              third + head > second
          ) {
            1
          } else {
            check(second +: third +: tail)
          }
        case _ => 0
      }
    }
    check(a.sorted.toVector.map(_.toLong))
  }
}

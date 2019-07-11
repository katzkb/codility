package lesson3

// https://app.codility.com/demo/results/trainingM83FDK-AQB/
object Q3TapeEquilibrium {
  def solution(a: Array[Int]): Int = {
    // write your code in Scala 2.12
    def f(resOpt: Option[Int], exSum: Int, remainSum: Int, va: Vector[Int]): Int = {
      va match {
        case head +: second +: tail =>
          val newExSum = exSum + head
          val newRemainSum = remainSum - head
          val diff = Math.abs(newExSum - newRemainSum)
          val newResOpt = resOpt match {
            case Some(newRes) => Some(Math.min(newRes, diff))
            case None         => Some(diff)
          }
          f(newResOpt, newExSum, newRemainSum, second +: tail)
        case _ => resOpt.getOrElse(0)
      }
    }
    if (a.length == 2) {
      Math.abs(a.head - a.last)
    } else {
      val total = a.sum
      f(None, 0, total, a.toVector)
    }
  }
}
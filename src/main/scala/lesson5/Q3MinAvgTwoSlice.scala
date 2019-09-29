package lesson5

// https://app.codility.com/demo/results/trainingX3AM4Q-9DS/
object Q3MinAvgTwoSlice {
  def solution(a: Array[Int]): Int = {
    @scala.annotation.tailrec
    def check(remain: Vector[Double], minAvg: Double, minIndex: Int = 0, idx: Int = 0): Int = {
      remain match {
        case head +: second +: third +: tail =>
          val (newMinAvg, newMinIdx) = Seq(
            (minAvg, minIndex),
            ((head + second) / 2, idx),
            ((head + second + third) / 3, idx)
          ).minBy(_._1)
          check(second +: third +: tail, newMinAvg, newMinIdx, idx + 1)
        case head +: second +: tail =>
          val (newMinAvg, newMinIdx) = Seq(
            (minAvg, minIndex),
            ((head + second) / 2, idx)
          ).minBy(_._1)
          check(second +: tail, newMinAvg, newMinIdx, idx + 1)
        case _ => minIndex
      }
    }
    check(a.toVector.map(_.toDouble), a.sum.toDouble / a.length.toDouble)
  }
}

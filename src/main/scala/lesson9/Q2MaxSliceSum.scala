package lesson9

// https://app.codility.com/demo/results/trainingP9BB86-9TQ/
object Q2MaxSliceSum {
  def solution(a: Array[Int]): Int = {
    // write your code in Scala 2.12
    val aMax = a.max
    val maxSlice = a.toVector.foldLeft((0: Int, 0: Int))((z, n) => {
      val (maxEnding, maxSlice) = z
      val newMaxEnding = Math.max(0, maxEnding + n)
      val newMaxSlice  = Math.max(maxSlice, newMaxEnding)
      (newMaxEnding, newMaxSlice)
    })._2
    if (aMax > 0) maxSlice else aMax
  }
}

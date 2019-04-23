// https://app.codility.com/demo/results/trainingYS4TFK-RYX/
object Solution {
  def solution(a: Array[Int]): Int = {
    // write your code in Scala 2.12
    val n = a.length + 1
    val original = BigDecimal(0.5) * n * (n+1)
    original.toInt - a.sum
  }
}
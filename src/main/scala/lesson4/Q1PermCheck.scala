package lesson4

// https://app.codility.com/demo/results/trainingKJFU9M-YVC/
object Q1PermCheck {
  def solution(a: Array[Int]): Int = {
    // write your code in Scala 2.12
    val n = a.max
    val original = BigDecimal(0.5) * n * (n+1)
    if (a.distinct.length == a.length && original.toInt == a.sum && a.length == n) 1 else 0
  }
}

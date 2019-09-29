package lesson5

// https://app.codility.com/demo/results/trainingJ79AMX-4CS/
object Q4CountDiv {
  def solution(a: Int, b: Int, k: Int): Int = {
    val zero = if (a == 0) 1 else 0
    (b / k + zero) - Math.max(0, (a-1) / k)
  }
}

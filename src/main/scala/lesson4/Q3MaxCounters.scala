package lesson4

// https://app.codility.com/demo/results/trainingUCBAHJ-XZG/
object Q3MaxCounters {
  def solution(n: Int, a: Array[Int]): Array[Int] = {
    var counters = Array.ofDim[Int](n)
    var fill = 0
    var max = 0
    a.foreach(cur =>
      if (cur > n) {
        fill = max
      } else {
        counters(cur-1) = Math.max(counters(cur-1), fill) +1
        max = Math.max(counters(cur-1), max)
      }
    )
    counters.map(v => if (v < fill) fill else v)
  }
}

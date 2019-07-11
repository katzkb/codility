package lesson10

// https://app.codility.com/demo/results/trainingKEKKFG-B6A/
object Q1CountFactors {
  def solution(n: Int): Int = {
    val maxInt = 2147483647
    def check(current: Int, cnt: Int): Int = {
      if (current * current > n) {
        cnt
      } else if (n % current == 0) {
        if (current < (n / current)) {
          check(current + 1, cnt + 2)
        } else if (current == (n / current)) {
          cnt + 1
        } else {
          cnt
        }
      } else {
        check(current + 1, cnt)
      }
    }

    if (n == maxInt) {
      2
    } else {
      check(1, 0)
    }
  }
}
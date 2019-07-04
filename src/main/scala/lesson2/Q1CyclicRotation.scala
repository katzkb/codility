// https://app.codility.com/demo/results/trainingK2WDVW-N9Y/
object Q1CyclicRotation {
  def solution(a: Array[Int], k: Int): Array[Int] = {
    // write your code in Scala 2.12
    if (k == 0 || a.isEmpty) a else solution(a.last +: a.init, k - 1)
  }
}

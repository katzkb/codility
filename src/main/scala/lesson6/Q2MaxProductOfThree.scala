// https://app.codility.com/demo/results/trainingJM6N8E-T4T/
object Q2MaxProductOfThree {
  def solution(a: Array[Int]): Int =  {
    // write your code in Scala 2.12
    val sortedA = a.sorted
    val sizeA = a.length
    math.max(
      sortedA.last * sortedA(sizeA - 2) * sortedA(sizeA - 3),
      sortedA.head * sortedA(1) * sortedA.last
    )
  }
}
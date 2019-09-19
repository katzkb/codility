package lesson9

// https://app.codility.com/demo/results/trainingXFS3Z4-PYU/
object Q3MaxDoubleSliceSum {
  def solution(a: Array[Int]): Int = {
    // write your code in Scala 2.12
    val va = a.toVector.tail.init
    val vaReverse = va.reverse
    val lastPosition = va.length

    val range = 0 until lastPosition
    val vInits = range.foldLeft(Vector(0))((z, n) => z :+ math.max(z(n) + va(n), 0) )
    val vTails = range.foldLeft(Vector(0))((z, n) => z :+ math.max(z(n) + vaReverse(n), 0) ).reverse

    (0 until lastPosition).foldLeft(0)((z, n) => {
      math.max(z, vInits(n) + vTails(n+1))
    })
  }
}

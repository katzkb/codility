package lesson9

object Q3MaxDoubleSliceSum {
  def solution(a: Array[Int]): Int = {
    val va = a.toVector.tail.init
    val vaReverse = va.reverse
    val lastPosition = va.length

    val range = 0 until lastPosition
    val vInits = range.foldLeft(Vector(0))((z, n) => z :+ math.max(z(n) + va(n), 0) )
    val vTails = range.foldLeft(Vector(0))((z, n) => z :+ math.max(z(n) + vaReverse(n), 0) ).reverse

    (1 until lastPosition).foldLeft(0)((z, n) => {
      math.max(z, vInits(n) + vTails(n+1))
    })
  }
}

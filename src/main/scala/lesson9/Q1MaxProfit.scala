package lesson9
// https://app.codility.com/demo/results/training7S5XKY-QTU/
object Q1MaxProfit {
  def solution(a: Array[Int]): Int = {
    // write your code in Scala 2.12
    def f(remain: Vector[Int], min: Int, res: Int): Int = {
      remain match {
        case head +: tail => {
          val newMin = Math.min(head, min)
          f(tail, newMin, Math.max(head - newMin, res))
        }
        case _ => res
      }
    }
    a.headOption match {
      case Some(head) => f(a.toVector, head, 0)
      case None       => 0
    }
  }
}

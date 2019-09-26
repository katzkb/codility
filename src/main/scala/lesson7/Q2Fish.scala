package lesson7

// https://app.codility.com/demo/results/trainingCHECRU-B8T/
object Q2Fish {
  def solution(a: Array[Int], b: Array[Int]): Int = {
    // write your code in Scala 2.12
    @scala.annotation.tailrec
    def upstream(
      remain: Vector[(Int, Int)],
      fish: (Int, Int)
    ): Vector[(Int, Int)] = {
      remain match {
        case init :+ last if last._2 == 1 =>
          if (last._1 > fish._1) {
            remain
          } else {
            upstream(init, fish)
          }
        case _ => remain :+ fish
      }
    }

    @scala.annotation.tailrec
    def downstream(
      remain: Vector[(Int, Int)],
      stack:  Vector[(Int, Int)] = Vector.empty
    ): Int = {
      remain match {
        case head +: tail =>
          if (head._2 == 0) {
            downstream(tail, upstream(stack, head))
          } else {
            downstream(tail, stack :+ head)
          }
        case _ => stack.length
      }
    }

    downstream(a.zip(b).toVector)
  }
}

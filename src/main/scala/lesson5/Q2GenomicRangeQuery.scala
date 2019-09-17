package lesson5

// https://app.codility.com/demo/results/training5KKJ3V-FEM/
object Q2GenomicRangeQuery {
  def solution(s: String, p: Array[Int], q:Array[Int]):Array[Int] = {
    val sArray = s.split("")
    val numArray = p.indices.toArray
    val countVec = sArray.foldLeft(Vector(Map("A" -> 0, "C" -> 0, "G" -> 0, "T" -> 0)))((z, s) =>
      z :+ z.last + (s -> (z.last(s) + 1))
    )

    for { i <- numArray } yield {
      val piMap = countVec(p(i))
      val qiMap = countVec(q(i) + 1)
      if (qiMap("A") - piMap("A") > 0) {
        1
      } else if (qiMap("C") - piMap("C") > 0) {
        2
      } else if (qiMap("G") - piMap("G") > 0) {
        3
      } else {
        4
      }
    }
  }
}

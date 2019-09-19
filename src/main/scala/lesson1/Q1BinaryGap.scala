package lesson1

// https://app.codility.com/demo/results/trainingV4C9Q9-28E/
object Q1BinaryGap {
  def solution(n: Int): Int = {
    n.toBinaryString.reverse.dropWhile(_ == '0').reverse.split("1").sortBy(_.length).reverse.headOption match {
      case Some(res) if res != "" => res.length
      case _ => 0
    }
  }
}

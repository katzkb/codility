package lesson3

// https://app.codility.com/demo/results/trainingED8NA7-926/
object Q1FrogJmp {
  def solution(x: Int, y: Int, d: Int): Int = {
    (y - x) / d + ((y - x) % d).signum
  }
}

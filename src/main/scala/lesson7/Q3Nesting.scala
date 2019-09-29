package lesson7

// https://app.codility.com/demo/results/training4JG83M-FV2/
object Q3Nesting {
  def solution(s: String): Int = {
    // write your code in Scala 2.12
    @scala.annotation.tailrec
    def check(stack: Vector[String], strVec: Vector[Char]): Int =
      strVec match {
        case h +: tail => h.toString match {
          case "("                                   => check(stack :+ h.toString, tail)
          case ")" if stack.lastOption.contains("(") => check(stack.init, tail)
          case ")"                                   => 0
          case _                                     => check(stack, tail)
        }
        case _ if stack.isEmpty => 1
        case _ => 0
      }
    check(Vector.empty[String], s.toVector)
  }
}

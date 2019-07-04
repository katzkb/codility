// https://app.codility.com/demo/results/trainingQP3C86-YPX/
import scala.annotation.tailrec

object Q1Brackets {
  def solution(s: String): Int = {
    // write your code in Scala 2.12
    val matchMap = Map(
      "}" -> "{",
      "]" -> "[",
      ")" -> "("
    )
    @tailrec
    def check(stack: Vector[String], strVec: Vector[Char]): Int =
      strVec match {
        case h +: t => matchMap.get(h.toString) match {
          case Some(v) if stack.lastOption.contains(v) => check(stack.init, t)
          case None                                    => check(stack :+ h.toString, t)
          case _ => 0
        }
        case _ if stack.isEmpty => 1
        case _ => 0
      }

    check(Vector.empty[String], s.toVector)
  }
}
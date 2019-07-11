package lesson7

// https://app.codility.com/demo/results/trainingG78PES-VHQ/
import scala.annotation.tailrec

object Q4StoneWall {
  def solution(h: Array[Int]): Int = {
    @tailrec
    def f(sum: Int, h: Vector[Int], stack: Vector[Int]): Int = {
      @tailrec
      def pop(s: Vector[Int]): Vector[Int] =
        if (s.nonEmpty && s.last > h.head) pop(s.init) else s

      val poppedStack = pop(stack)
      val needStack  = poppedStack.isEmpty || poppedStack.last != h.head
      val newSum     = if (needStack) sum + 1 else sum
      val newStack   = if (needStack) poppedStack :+ h.head else poppedStack

      val newH = h.tail
      if (newH.nonEmpty) f(newSum, newH, newStack) else newSum
    }
    f(0, h.toVector, Vector(0))
  }
}
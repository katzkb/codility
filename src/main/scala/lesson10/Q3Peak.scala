package lesson10

// https://app.codility.com/demo/results/trainingDYNEYU-9G6/
object Q3Peak {
  def solution(a: Array[Int]): Int = {
    val size = a.length

    // すべての中からピークのインデックスリストを取得
    @scala.annotation.tailrec
    def getAllPeaks(remain: Vector[(Int, Int)], res: Vector[Int] = Vector.empty[Int]): Vector[Int] = {
      remain match {
        case prev +: cur +: next +: tail =>
          val peakIdx = if (prev._1 < cur._1 && cur._1 > next._1) Vector(cur._2) else Vector.empty[Int]
          getAllPeaks(cur +: next +: tail, res ++ peakIdx)
        case prev +: cur +: next +: _  =>
          val peakIdx = if (prev._1 < cur._1 && cur._1 > next._1) Vector(cur._2) else Vector.empty[Int]
          res ++ peakIdx
        case _ => res
      }
    }
    val peaks = getAllPeaks(a.toVector.zipWithIndex)

    // 次のブロック以降のピークのインデックスリストを取得
    @scala.annotation.tailrec
    def getOnlyNextBlock(p: Vector[Int], blockEnd: Int): Vector[Int] =
      p match {
        case head +: _ if head >= blockEnd => p
        case _ +: tail                     => getOnlyNextBlock(tail, blockEnd)
        case _                             => Vector.empty[Int]
      }

    // 指定されたブロック数に分割できるかどうか
    @scala.annotation.tailrec
    def hasPeaks(peakV: Vector[Int], blockNum: Int, cnt: Int = 1): Boolean = {
      val blockEnd = blockNum * cnt
      peakV match {
        case head +: tail =>
          if (head < blockEnd) {
            if (tail.isEmpty) {
              size <= blockEnd
            } else {
              hasPeaks(getOnlyNextBlock(tail, blockEnd), blockNum, cnt + 1)
            }
          } else false
        case _ => size < blockEnd
      }
    }

    // 分割できる数でデクリメントしていく
    @scala.annotation.tailrec
    def getBlockNum(i: Int): Int = {
      if (i <= 0) {
        0
      } else {
        if (size % i == 0) {
          if (hasPeaks(peaks, size / i)) i else getBlockNum(i - 1)
        } else {
          getBlockNum(i - 1)
        }
      }
    }
    getBlockNum(peaks.length)
  }
}

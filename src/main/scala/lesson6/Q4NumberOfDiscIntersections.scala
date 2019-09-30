package lesson6

// https://app.codility.com/demo/results/training7XABAC-RGT/
object Q4NumberOfDiscIntersections {
  def solution(a: Array[Int]): Int = {
    val size = a.length.toLong
    val (_, openEvents, closeEvents) = a.foldLeft(0L, Map.empty[Long, Long], Map.empty[Long, Long])((z, n) => {
      val (k, left, right) = z
      // 左端と右端の位置を取得
      val (li, ri) = (-n.toLong + k, n.toLong + k)
      // 右端は必ず正の値になるので、左端が負の場合は0とみなしてよい
      val ll = if (li < 0L)               0L else li
      // 最後のインデックス以降に存在する右端は必ず重複するので、見るのはsizeまでで良い
      val rr = if (ri > size - 1L) size - 1L else ri
      // それぞれの位置での端の出現回数をカウント
      val leftMap  = left  + (ll -> (left.getOrElse(ll, 0L)  + 1L))
      val rightMap = right + (rr -> (right.getOrElse(rr, 0L) + 1L))
      (k + 1L, leftMap, rightMap)
    })

    val res = (0L until size).foldLeft(0L, 0L)((z, n) => {
      val (opened, intersection) = z
      // 当該位置での端の出現回数を取得
      val openEvent  = openEvents.getOrElse(n, 0L)
      val closeEvent = closeEvents.getOrElse(n, 0L)
      // 開始位置の出現回数を更新
      val newOpened = opened + openEvent
      // いずれかの端が存在しない場合は次へ
      if (newOpened == 0L || closeEvent == 0L) {
        (newOpened, intersection)
      } else {
        // 端が存在する場合は現時点での開始位置出現回数と今回の終了位置から重複数(等差数列で求める)を取得
        val curIntersection = intersection + Math.max(0L, closeEvent * (2L * newOpened - 1L - closeEvent) / 2L)
        (newOpened - closeEvent, curIntersection)
      }
    })._2
    if (res > 10000000) -1 else res.toInt
  }
}

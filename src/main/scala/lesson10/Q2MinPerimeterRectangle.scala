package lesson10

// https://app.codility.com/demo/results/trainingPA2TV4-BFE/
object Q2MinPerimeterRectangle {
  def solution(n: Int): Int = {
    @scala.annotation.tailrec
    def check(minFactor: Int = 1, cnt: Int = 1): Int = {
      val factor = n / cnt
      if (n % cnt == 0) {
        val newMinFactor = Math.min(cnt, minFactor)
        if (cnt < factor) {
          check(newMinFactor, cnt + 1)
        } else {
          getPerimeter(cnt, factor)
        }
      } else {
        check(minFactor, cnt + 1)
      }
    }

    // 周囲を取得する式
    def getPerimeter(x: Int, y: Int): Int = (x + y) * 2

    // 素数確認
    def isPrime(num: Int): Boolean =
      (num != 1 && num % 2 != 0) && Range(3, Math.sqrt(num).toInt, 2).forall(num % _ != 0)

    if (isPrime(n)) getPerimeter(n, 1) else check()
  }
}

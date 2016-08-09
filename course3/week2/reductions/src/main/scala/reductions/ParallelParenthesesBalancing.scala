package reductions

import scala.annotation._
import org.scalameter._
import common._

object ParallelParenthesesBalancingRunner {

  @volatile var seqResult = false

  @volatile var parResult = false

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 40,
    Key.exec.maxWarmupRuns -> 80,
    Key.exec.benchRuns -> 120,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val length = 100000000
    val chars = new Array[Char](length)
    val threshold = 10000
    val seqtime = standardConfig measure {
      seqResult = ParallelParenthesesBalancing.balance(chars)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential balancing time: $seqtime ms")

    val fjtime = standardConfig measure {
      parResult = ParallelParenthesesBalancing.parBalance(chars, threshold)
    }
    println(s"parallel result = $parResult")
    println(s"parallel balancing time: $fjtime ms")
    println(s"speedup: ${seqtime / fjtime}")
  }
}

object ParallelParenthesesBalancing {

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def balance(chars: Array[Char]): Boolean = {
    var i, count = 0
    var stop = false
    while ((i < chars.length) && !stop) {
      if (chars(i) == '(') count += 1
      else if (chars(i) == ')') {
        count -= 1
        if (count < 0) stop = true
      }
      i += 1
    }
    count == 0
  }

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {

    def traverse(idx: Int, until: Int): (Int, Int) = {
      var counter = 0
      var minCounter = Int.MaxValue
      var i = idx
      while (i < until) {
        if (chars(i) == '(') counter += 1
        else if (chars(i) == ')') {
         counter -= 1
         if (counter < minCounter) minCounter = counter
        }
        i += 1
      }
      (counter, if (minCounter == Int.MaxValue) 0 else minCounter)
    }

    def reduce(from: Int, until: Int) :(Int, Int) = {
      if (until - from <= threshold) traverse(from, until)
      else {
        val mid = from + (until - from) / 2
        val (resL, resR) = parallel(reduce(from, mid), reduce(mid, until))
        (resL._1 + resR._1, math.min(resL._2, resL._1 + resR._2))
      }
    }

    reduce(0, chars.length) == (0, 0)
  }

  // For those who want more:
  // Prove that your reduction operator is associative!

}

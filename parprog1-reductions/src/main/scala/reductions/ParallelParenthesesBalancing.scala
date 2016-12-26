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

  /**
   * Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def balance(chars: Array[Char]): Boolean = {
    val end = chars.length
    def recursiveBalance(acc: Int, index: Int): Boolean = {
      if (index >= end) acc == 0
      else if (chars(index) == ')' && acc == 0) false
      else {
        val acc2 = chars(index) match {
          case '(' => 1
          case ')' => -1
          case _ => 0
        }
        recursiveBalance(acc + acc2, index + 1)
      }
    }
    
    recursiveBalance(0,0);
// version doesn't stop if 'acc' is the first time lower than 0
//    if (chars.isEmpty) true
//    else
//      0 == chars
//        .map({
//        })
//        ./:(0)((a, b) => if (a + b < 0) Int.MinValue else a + b)
        
  }

  /**
   * Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {

    def traverse(idx: Int, until: Int, arg1: Int, arg2: Int): (Int, Int) = {
      var minNest = 0
      var totNest = 0
      for (current <- idx until until) {
        if (chars(current) == '(') totNest = totNest + 1
        if (chars(current) == ')') totNest = totNest - 1
        if (totNest < minNest) minNest = totNest
      }

      (minNest, totNest)
    }

    def reduce(from: Int, until: Int): (Int, Int) = {
      if (until - from <= threshold) 
        traverse(from, until, 0, 0)
      else {
        val middle = from + (until - from) / 2
        val (resultLeft, resultRight) = parallel(reduce(from, middle), reduce(middle, until))
        (Math.min(resultLeft._1, resultLeft._2 + resultRight._1), resultLeft._2 + resultRight._2)
      }
    }

    reduce(0, chars.length) == (0, 0)
  }

  // For those who want more:
  // Prove that your reduction operator is associative!

}

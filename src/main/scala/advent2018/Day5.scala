package advent2018

import scalaadventutils.Problem

import annotation.tailrec
import scala.math.abs

object Day5 {

    def main(args: Array[String]) {
        val start = Problem.parseInputToString("day5")
        println(runReactions(start))
        println(remove(start))
    }

    def remove(p: String) =
        ('a' to 'z').map(ch => {
            val chStr = ch.toString
            runReactions(p.replace(chStr, "").replace(chStr.toUpperCase, ""))
        }).min

    def runReactions(p: String): Int = {
        def run_(pr: String): String = {
            val r = react(pr)
            if (r == pr) r else run_(r)
        }
        run_(p).size
    }

    def react(p: String) = {
        @tailrec
        def react_(i: Int): String = {
            if (i >= p.size - 1) p
            else if (abs(p(i) - p(i + 1)) == 32)
                p.slice(0, i) + p.slice(i + 2, p.size)
            else react_(i + 1)
        }
        react_(0)
    }
}

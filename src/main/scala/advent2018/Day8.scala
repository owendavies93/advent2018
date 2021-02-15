package advent2018

import scalaadventutils.Problem

import scala.collection.mutable.Queue

object Day8 {

    def main(args: Array[String]) {
        val values = Problem.parseInputLineToList("day8", " ").map(_.toInt)
        println(sumMeta(values))
        println(findValue(values))
    }

    /*
        XXX: horrible
    */
    def findValue(vals: List[Int]) = {
        var q = Queue[Int]()
        q ++= vals

        def find_ : Int = {
            val ch = q.dequeue()
            val me = q.dequeue()

            if (ch == 0)
                (1 to me).map(_ => q.dequeue()).sum
            else {
                val children = (1 to ch).map(_ => find_).toArray
                var total = 0
                (1 to me).foreach(_ => {
                    val md = q.dequeue()
                    if (md <= children.size)
                        total += children(md - 1)
                })
                total
            }
        }

        find_
    }

    def sumMeta(vals: List[Int]) = {
        var total = 0
        var q = Queue[Int]()
        q ++= vals

        def sum_ : Unit = {
            val ch = q.dequeue()
            val me = q.dequeue()

            (1 to ch).foreach(_ => sum_)
            (1 to me).foreach(_ => total += q.dequeue())
        }

        sum_
        total
    }

}

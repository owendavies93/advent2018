package advent2018

import scalaadventutils.Problem

object Day12 {

    type Notes = Map[List[Boolean], Boolean]

    def main(args: Array[String]) {
        val lines = Problem.parseInputToList("day12")
        println(run(lines, 3))
        println(run(lines, 150))
    }

    def run(lines: List[String], times: Int, target: Long = 50000000000L) = {
        val (ca, notes) = parseInput(lines)

        val result = (1 to times).foldLeft((ca, 0))((next, i) => {
            val (nextCa, nextZero) = next
            val (newCa, newZero)   = step(nextCa, notes, nextZero)

            if (nextCa == newCa) {
                val sum = sumPots(newCa, newZero)
                val diff = sum - sumPots(nextCa, nextZero)
                println(sum + (diff * (target - i)))
            }

            (newCa, newZero)
        })
        sumPots(result._1, result._2)
    }

    private def sumPots(ca: List[Boolean], zeroIndex: Int) =
        ca.zipWithIndex.filter(_._1).map(_._2 - zeroIndex).sum

    def step(ca: List[Boolean], n: Notes, zero: Int): (List[Boolean], Int) = {
        val nextGrid = List(false, false, false, false) ++
                       ca ++ List(false, false, false, false)

        val nextGen = nextGrid.sliding(5).map(s => n(s)).toList
        val start = nextGen.indexOf(true)
        val end = nextGen.lastIndexOf(true) + 1
        (nextGen.slice(start, end), zero + 2 - start)
    }

    def parseInput(lines: List[String]): (List[Boolean], Notes) = {
        val state = lines.head.split(": ")(1).map(_ == '#').toList

        val notes = lines.drop(2).map(l => {
            val s = l.split(" => ")
            s(0).map(_ == '#').toList -> (s(1) == "#")
        }).toMap.withDefaultValue(false)
        (state, notes)
    }
}

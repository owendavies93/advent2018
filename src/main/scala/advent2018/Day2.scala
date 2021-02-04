package advent2018

import scalaadventutils.Problem

object Day2 {

    def main(args: Array[String]) {
        val lines = Problem.parseInputToList("day2")
        println(part1(lines))
        println(part2(lines))
    }

    def part1(lines: List[String]) = countOcc(lines, 2) * countOcc(lines, 3)

    def part2(lines: List[String]) = {
        val res = lines.combinations(2).find(c =>
            (c(0) zip c(1)).count { case (a, b) => a != b } == 1
        ).get

        (res(0) zip res(1)).filter { case (a, b) => a == b }.map(_._1).mkString
    }

    def countOcc(lines: List[String], length: Int) =
        lines.count(l => l.groupBy(identity).values.exists(_.size == length))

}

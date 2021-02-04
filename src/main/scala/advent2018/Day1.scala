package advent2018

import scalaadventutils.Problem

object Day1 {

    def main(args: Array[String]) {
        val lines = Problem.parseInputToList("day1")
        println(part1(lines))
        println(part2(lines))
    }

    def part1(lines: List[String]) = lines.map(_.toInt).sum

    def part2(lines: List[String]) = {
        val freqs = Stream.continually(lines.map(_.toInt).toStream).flatten

        def findDupe(seen: Set[Int], i: Int, total: Int): Int =
            if (seen.contains(total)) total
            else findDupe(seen + total, i + 1, total + freqs(i))

        findDupe(Set[Int](), 0, 0)
    }

}

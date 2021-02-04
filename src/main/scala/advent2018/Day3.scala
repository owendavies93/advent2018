package advent2018

import scalaadventutils.Problem

object Day3 {
    val parser = """#(\d+) @ (\d+),(\d+): (\d+)x(\d+)""".r

    def main(args: Array[String]) {
        val lines = Problem.parseInputToList("day3")
        println(part1(lines))
        println(part2(lines))
    }

    def part1(lines: List[String]) =
        lines.flatMap(getPoints(_)._2).groupBy(identity).values
             .count(_.size > 1)

    def part2(lines: List[String]) = {
        val uniques = lines.flatMap(getPoints(_)._2).groupBy(identity).values
                           .filter(_.size == 1).flatten.toSet

        lines.map(getPoints).find {
            case (id, ps) => ps.forall(uniques.contains(_))
        }.get._1
    }

    private def getPoints(line: String) = line match {
        case parser(id, sX, sY, lX, lY) => (id.toInt,
            (sY.toInt until sY.toInt + lY.toInt).flatMap(y =>
                (sX.toInt until sX.toInt + lX.toInt).map(x => (x, y))))
    }
}

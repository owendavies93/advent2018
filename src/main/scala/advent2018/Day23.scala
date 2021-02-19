package advent2018

import scalaadventutils.Problem

import scala.math.abs

object Day23 {

    val parser = """pos=<(-?\d+),(-?\d+),(-?\d+)>, r=(\d+)""".r

    def main(args: Array[String]): Unit = {
        val lines = Problem.parseInputToList("day23")
        println(part1(lines))
    }

    def part1(lines: List[String]): Int = {
        val bots = parseInput(lines)

        val strongest = bots.maxBy(_.r)
        bots.count(b => b.distance(strongest) <= strongest.r)
    }

    def parseInput(lines: List[String]): List[Nanobot] =
        lines.map(l => l match {
            case parser(x, y, z, r) =>
                Nanobot(x.toInt, y.toInt, z.toInt, r.toInt)
        })

    case class Nanobot(x: Int, y: Int, z: Int, r: Int) {
        def distance(that: Nanobot) =
            abs(that.x - x) + abs(that.y - y) + abs(that.z -z)
    }
}

package advent2018

import scalaadventutils.Problem
import scalaadventutils.WeightedUndirectedGraph

import scala.math.abs

object Day25 {

    def main(args: Array[String]): Unit = {
        val lines = Problem.parseInputToList("day25")
        println(part1(lines))
    }

    def part1(lines: List[String]): Int = {
        val points = parseInput(lines)

        val graph = constructGraph(points)
        graph.countConnectedComponents
    }

    def constructGraph(ps: List[Point]): WeightedUndirectedGraph[Point] = {
        val g = ps.map(p => {
            val near = ps.filterNot(_ == p).filter(_.distance(p) <= 3)
            p -> near.map(_ -> 1).toMap
        }).toMap

        new WeightedUndirectedGraph(g)
    }

    def parseInput(lines: List[String]) =
        lines.map(l => {
            val coords = l.trim.split(",")
            Point(coords(0).toInt,
                  coords(1).toInt,
                  coords(2).toInt,
                  coords(3).toInt)
        })

    case class Point(x: Int, y: Int, z: Int, t: Int) {
        def distance(that: Point) =
            abs(that.x - x) +
            abs(that.y - y) +
            abs(that.z - z) +
            abs(that.t - t)
    }
}


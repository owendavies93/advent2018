package advent2018

import scalaadventutils.Problem

import scala.math.abs

object Day6 {

    def main(args: Array[String]) {
        val lines = Problem.parseInputToList("day6")
        println(findLargestArea(lines))
        println(findPointsBelowDistance(lines, 10000))
    }

    def findLargestArea(lines: List[String]) = {
        val points = parseInput(lines)
        val minX = points.minBy(_.x).x
        val minY = points.minBy(_.y).y
        val maxX = points.maxBy(_.x).x
        val maxY = points.maxBy(_.y).y

        val nearest = (minY to maxY).flatMap(y =>
            (minX to maxX).map(x =>
                Point(x, y) -> Point(x, y).nearest(points))).toMap
            .filterNot(_._2 == None)
            .mapValues(_.get)

        val edgePoints = nearest.filterKeys(p =>
            p.x == minX || p.x == maxX || p.y == minY || p.y == maxY
        ).values.toSet

        nearest.values
               .filterNot(edgePoints.contains(_))
               .groupBy(identity).values
               .map(_.size)
               .max
    }

    def findPointsBelowDistance(lines: List[String], target: Int) = {
        val points = parseInput(lines)
        val minX = points.minBy(_.x).x
        val minY = points.minBy(_.y).y
        val maxX = points.maxBy(_.x).x
        val maxY = points.maxBy(_.y).y

        val allPoints = (minY to maxY).flatMap(y =>
            (minX to maxX).map(x => Point(x, y)))

        allPoints.filter(_.totalDistance(points) < target).size
    }

    def parseInput(lines: List[String]) =
        lines.map(l => {
            val coords = l.split(", ")
            Point(coords(0).toInt, coords(1).toInt)
        })

    case class Point(x: Int, y: Int) {
        def nearest(ps: List[Point]): Option[Point] = {
            val distances = ps.map(p => (p, distanceTo(p)))
            val minDist = distances.minBy(_._2)
            if (distances.count(_._2 == minDist._2) == 1)
                Some(minDist._1)
            else None
        }

        def totalDistance(ps: List[Point]) = ps.map(distanceTo).sum

        def distanceTo(p: Point) = abs(p.x - x) + abs(p.y - y)
    }
}

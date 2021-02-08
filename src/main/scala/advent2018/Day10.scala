package advent2018

import scalaadventutils.Problem

import scala.math.abs

object Day10 {

    val parser = """position=<\s*(-?\d+),\s*(-?\d+)> velocity=<\s*(-?\d+),\s*(-?\d+)>""".r

    def main(args: Array[String]) {
        val lines = Problem.parseInputToList("day10")
        step(25000, lines)
    }

    def step(times: Int, lines: List[String]) {
        val ps = parseInput(lines)

        val bounds = (0 until times).scanLeft((ps, Int.MaxValue))(
            (next, i) => {
                val (points, _) = next
                val newPoints = points.map(_.step)
                val xs = newPoints.map(_.x)
                val ys = newPoints.map(_.y)
                (newPoints, (xs.max - xs.min) + (ys.max - ys.min))
            }
        )

        val smallest = bounds.zipWithIndex.minBy(_._1._2)
        draw(smallest._1._1)
        println(smallest._2)
    }

    def draw(ps: List[Point]) {
        val xs = ps.map(_.x)
        val ys = ps.map(_.y)
        val pm = ps.map(p => (p.x, p.y) -> 1).toMap

        val out =
            (ys.min to ys.max).map(y =>
                (xs.min to xs.max).map(x =>
                    if (pm.contains((x, y))) '#' else '.'
                ).mkString
            ).mkString("\n")

        println(out)
    }

    def parseInput(lines: List[String]) = lines map(l => {
        l match {
            case parser(x, y, vx, vy) => {
                Point(x.toInt, y.toInt, Vector(vx.toInt, vy.toInt))
            }
        }
    })

    case class Point(x: Int, y: Int, v: Vector[Int]) {
        def step = copy(x = x + v(0), y = y + v(1))
    }
}

package advent2018

import scalaadventutils.Problem

import annotation.tailrec
import scala.collection.mutable.PriorityQueue
import scala.math.abs

object Day23 {

    val parser = """pos=<(-?\d+),(-?\d+),(-?\d+)>, r=(\d+)""".r

    def main(args: Array[String]): Unit = {
        val lines = Problem.parseInputToList("day23")
        println(part1(lines))
        println(part2(lines))
    }

    def part1(lines: List[String]): Int = {
        val bots = parseInput(lines)

        val strongest = bots.maxBy(_.r)
        bots.count(b => b.distance(strongest) <= strongest.r)
    }

    def part2(lines: List[String]): Int = {
        val bots = parseInput(lines)
        val centre = threeDCentre(bots.map(_.l))
        val bounds = Bounds(centre, bots.map(_.l.distance(centre)).max)

        @tailrec
        def findCentreOfLargestSubBox(q: PriorityQueue[(Bounds, Int)]): Point = {
            val (b, _) = q.dequeue()
            if (b.r == 0) b.centre
            else {
                val split = b.split.map(b_ => (b_, b_.reachable(bots)))
                split.foreach(q.enqueue(_))
                findCentreOfLargestSubBox(q)
            }
        }

        val initial =
            PriorityQueue((bounds, bounds.reachable(bots)))(Ordering.by(_._2))

        findCentreOfLargestSubBox(initial).distance(Point(0, 0, 0))
    }

    private def threeDCentre(ps: List[Point]): Point = {
        val xs = ps.map(_.x)
        val ys = ps.map(_.y)
        val zs = ps.map(_.z)
        Point(oneDCentre(xs), oneDCentre(ys), oneDCentre(zs))
    }

    private def oneDCentre(ns: List[Int]): Int = {
        val min = ns.min
        min + (ns.max - ns.min) / 2
    }

    def parseInput(lines: List[String]): List[Nanobot] =
        lines.map(l => l match {
            case parser(x, y, z, r) =>
                Nanobot(Point(x.toInt, y.toInt, z.toInt), r.toInt)
        })

    case class Point(x: Int, y: Int, z: Int) {
        def distance(that: Point) =
            abs(that.x - x) + abs(that.y - y) + abs(that.z -z)
    }

    case class Nanobot(l: Point, r: Int) {
        def distance(that: Nanobot) = l.distance(that.l)
    }

    case class Bounds(centre: Point, r: Int) {
        def reachable(bots: List[Nanobot]) = bots.count(b =>
            centre.distance(b.l) <= r + b.r
        )

        def split: List[Bounds] = {
            val newR = (r) / 3
            (-1 to 1).flatMap(z =>
                (-1 to 1).flatMap(y =>
                    (-1 to 1).map(x => {
                        val newC = Point(
                            centre.x + x * newR,
                            centre.y + y * newR,
                            centre.z + z * newR
                        )
                        Bounds(newC, newR)
                    })
                )
            ).toList
        }
    }
}

package advent2018

import annotation.tailrec

object Day22 {

    type Caves = Map[Point, Type]
    type GL = Map[Point, Int]

    def main(args: Array[String]): Unit = {
        println(part1(Point(7, 782), 11820))
    }

    def part1(target: Point, depth: Int): Int = {
        val caves = constructCave(target, depth)

        val maxx = target.x
        val maxy = target.y

        (0 to maxy).flatMap(y =>
            (0 to maxx).map(x =>
                caves(Point(x, y)).risk
            )
        ).sum
    }

    def constructCave(target: Point, depth: Int): Caves = {
        val maxx = target.x
        val maxy = target.y

        val geologicLevels: GL =
            (0 to maxx).map(x => Point(x, 0) -> x * 16807).toMap ++
            (0 to maxy).map(y => Point(0, y) -> y * 48271).toMap


        val erosions = collection.mutable.Seq.fill(maxy + 1, maxx + 1)(-1)

        (0 to maxy).map(y =>
            (0 to maxx).map(x => {
                val geologicIndex = (y, x) match {
                    case (0, 0) => 0
                    case (y, x) if x == target.x && y == target.y => 0
                    case (0, x) => x * 16807
                    case (y, 0) => y * 48271
                    case (y, x) => erosions(y)(x - 1) * erosions(y - 1)(x)
                }

                erosions(y)(x) = (geologicIndex + depth) % 20183
            })
        )

        val allErosionLevels = erosions.toVector.map(_.toVector)

        (0 to maxy).flatMap(y =>
            (0 to maxx).map(x => {
                val e = allErosionLevels(y)(x)
                val t: Type = e match {
                    case i if i % 3 == 0 => Rocky
                    case i if i % 3 == 1 => Wet
                    case i if i % 3 == 2 => Narrow
                }
                Point(x, y) -> t
            })
        ).toMap
    }

    case class Point(x: Int, y: Int)

    sealed trait Type {
        def risk: Int
    }

    case object Rocky extends Type {
        def risk = 0
        override def toString() = "."
    }

    case object Wet extends Type {
        def risk = 1
        override def toString() = "="
    }

    case object Narrow extends Type {
        def risk = 2
        override def toString() = "|"
    }
}

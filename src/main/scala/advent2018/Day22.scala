package advent2018

import scalaadventutils.Dijkstra
import scalaadventutils.WeightedUndirectedGraph

import annotation.tailrec

object Day22 {

    type Caves = Map[Point, Type]
    type GL    = Map[Point, Int]
    type State = (Type, Equipment)
    type Graph = Map[(Point, Equipment), Map[(Point, Equipment), Int]]

    private val nonDiagNeighbourList = List(
        (-1, 0), (0, -1), (1, 0), (0, 1)
    )

    def main(args: Array[String]): Unit = {
        println(part1(Point(7, 782), 11820))
        println(part2(Point(7, 782), 11820))
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

    def part2(target: Point, depth: Int): Int = {
        val caves = constructCave(target, depth)
        val graph = constructGraph(caves, target)

        val path = Dijkstra.shortestPath(
            graph, (Point(0, 0), Torch), (target, Torch))
        Dijkstra.shortestPathTotalWeight(graph, path)
    }

    def constructCave(target: Point, depth: Int): Caves = {
        val maxx = target.x * 2
        val maxy = target.y * 2

        val geologicLevels: GL =
            (0 to maxx).map(x => Point(x, 0) -> x * 16807).toMap ++
            (0 to maxy).map(y => Point(0, y) -> y * 48271).toMap


        val erosions = collection.mutable.Seq.fill(maxy + 1, maxx + 1)(-1)

        (0 to maxy).map(y =>
            (0 to maxx).map(x => {
                val geoIndex = (x, y) match {
                    case (0, 0) => 0
                    case (x, y) if x == target.x && y == target.y => 0
                    case (0, y) => y * 48271
                    case (x, 0) => x * 16807
                    case (x, y) => erosions(y)(x - 1) * erosions(y - 1)(x)
                }

                erosions(y)(x) = (geoIndex + depth) % 20183
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

    def constructGraph
        ( c: Caves
        , target: Point)
        : WeightedUndirectedGraph[(Point, Equipment)] = {

        val maxx = target.x * 2
        val maxy = target.y * 2

        val graph: Graph =
        (0 to maxy).flatMap(y =>
            (0 to maxx).map(x => {
                val p = Point(x, y)
                val regionType = c(p)

                regionType.usable.map(e => {
                    // TODO: remove ugly mutability here
                    val edges = collection.mutable.Map[(Point, Equipment), Int]()

                    (regionType, e) match {
                        case (Rocky, Torch)    => edges += (p, Climbing) -> 7
                        case (Rocky, Climbing) => edges += (p, Torch)    -> 7
                        case (Wet, None)       => edges += (p, Climbing) -> 7
                        case (Wet, Climbing)   => edges += (p, None)     -> 7
                        case (Narrow, None)    => edges += (p, Torch)    -> 7
                        case (Narrow, Torch)   => edges += (p, None)     -> 7
                        case _ =>
                    }

                    val neighbours = nonDiagNeighbours(p, maxx, maxy)
                    neighbours.map(n => {
                        val nRegionType = c(n)
                        if (nRegionType.usable.contains(e))
                            edges += (n, e) -> 1
                    })
                    (p, e) -> edges.toMap
                })
            })
        ).flatten.toMap

        new WeightedUndirectedGraph(graph)
    }

    def nonDiagNeighbours(p: Point, maxx: Int, maxy: Int) =
        nonDiagNeighbourList.map(n => Point(p.x + n._1, p.y + n._2))
                            .filter(p => p.x >= 0 && p.x <= maxx &&
                                         p.y >= 0 && p.y <= maxy)

    case class Point(x: Int, y: Int)

    sealed trait Type {
        def risk: Int
        def usable: List[Equipment]
    }

    case object Rocky extends Type {
        def risk = 0
        override def toString() = "."
        def usable = List(Climbing, Torch)
    }

    case object Wet extends Type {
        def risk = 1
        override def toString() = "="
        def usable = List(Climbing, None)
    }

    case object Narrow extends Type {
        def risk = 2
        override def toString() = "|"
        def usable = List(None, Torch)
    }

    sealed trait Equipment

    case object Torch extends Equipment

    case object Climbing extends Equipment

    case object None extends Equipment
}


package advent2018

import scalaadventutils.Dijkstra
import scalaadventutils.Problem
import scalaadventutils.Grid
import scalaadventutils.GridUtils
import scalaadventutils.WeightedUndirectedGraph

import collection.mutable.ArrayBuffer

object Day15 {

    type Caves = Map[Point, Map[Point, Int]]

    case class Point(x: Int, y: Int) {
        override def toString() = "(" + x + "," + y + ")"
    }

    object Type extends Enumeration {
        val Goblin, Elf = Value
    }

    case class Character(pos: Point, side: Type.Value, hp: Int) {
        def nextToTarget(allChars: List[Character], g: Grid) =
            allChars
                .filter(_.side != side)
                .filter(t =>
                    g.nonDiagNeighbours(t.pos.x, t.pos.y, false)
                     .filter(p => Point(p._1, p._2) == pos)
                     .size > 0
                ).size > 0

        def move
            ( allChars: List[Character]
            , g: Grid
            , graph: WeightedUndirectedGraph[Point]) = {

            val targets = allChars.filter(_.side != side)
            val allLocs = allChars.map(_.pos).filterNot(_ == pos)

            val candidateLocs = targets.flatMap(t =>
                g.nonDiagNeighbours(t.pos.x, t.pos.y, false)
                 .filter(p => g.get(p._1, p._2) == true)
            ).distinct

            if (candidateLocs.isEmpty || nextToTarget(allChars, g)) this
            else {
                /*
                    Caculate shortest paths from the current location to the
                    candidate locations after excluding other character
                    locations from the graph
                */
                val usableGraph = allLocs.foldLeft(graph)(
                    (next, loc) => next.removeEdgesTo(loc))

                val validPaths = candidateLocs
                    .map(l => Dijkstra.shortestPath[Point](
                        usableGraph, pos, Point(l._1, l._2)))
                    .filter(p => p.contains(pos))

                /*
                    All edge weights are 1 so we just need the shortest path,
                    not the shortest total weight (or rather, they are
                    equivalent and the former is simpler)
                */
                val groupedPaths = validPaths.groupBy(_.size)
                val shortestPaths = groupedPaths(groupedPaths.keys.min)

                val chosen = shortestPaths
                    .sortBy(p => (p.last.y, p.last.x))
                    .head.drop(1).head

                copy(pos = chosen)
            }
        }

        // TODO: attack

        override def toString() =
            (if (side == Type.Goblin) "G" else "E") + "-" + pos.toString
    }

    def play(lines: List[String]) {
        val (map, goblins, elves) = parseInput(lines)
        val graph = new WeightedUndirectedGraph(constructGraph(map))

        def turn(gs: List[Character], es: List[Character], rounds: Int): Int = {
            if (gs.size == 0)
                return rounds * es.map(_.hp).sum
            else if (es.size == 0)
                return rounds * gs.map(_.hp).sum

            val all = (gs ++ es).sortBy(ch => (ch.pos.y, ch.pos.x))

            // TODO: game loop

            0
        }
    }

    def constructGraph(grid: Grid) =
        (0 until grid.height).flatMap(y => {
            (0 until grid.width).map(x => {
                val openNeighbours =
                    grid.nonDiagNeighbours(x, y)
                        .filter(n => grid.get(n._1, n._2) == true)
                (Point(x, y),
                    openNeighbours.map(p => (Point(p._1, p._2), 1)).toMap)
            })
        }).toMap

    def parseInput
        ( lines: List[String])
        : (Grid, List[Character], List[Character]) = {

        val goblins =
            pointMap(lines).filter(_._2 == 'G').map {
                case (p, _) => Character(p, Type.Goblin, 200)
            }.toList

        val elves =
            pointMap(lines).filter(_._2 == 'E').map {
                case (p, _) => Character(p, Type.Elf, 200)
            }.toList

        val emptyMap = lines.map(l => l.map(ch => ch match {
            case 'E' => '.'
            case 'G' => '.'
            case c   => c
        }))

        val grid = GridUtils.from2DCharArray(emptyMap, '.')
        (grid, goblins, elves)
    }

    def printMap(grid: Grid, chars: List[Character]) {
        val charMap = chars.map(ch => ch.pos -> ch.side).toMap

        println(
            (0 until grid.height).map(y =>
                (0 until grid.width).map(x =>
                    if (charMap.contains(Point(x,y)))
                        if (charMap(Point(x,y)) == Type.Elf) 'E' else 'G'
                    else
                        if (grid.get(x, y)) '.' else '#'
                ).mkString
            ).mkString("\n")
        )
        println()
    }

    private def pointMap(lines: List[String]) = {
        val height = lines.size
        val width  = lines(0).size

        (0 until height).flatMap(y =>
            (0 until width).map(x =>
                Point(x, y) -> lines(y)(x)
            )
        )
    }
}

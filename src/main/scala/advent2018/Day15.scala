package advent2018

import scalaadventutils.Dijkstra
import scalaadventutils.Problem
import scalaadventutils.Grid
import scalaadventutils.GridUtils
import scalaadventutils.WeightedUndirectedGraph

import collection.mutable.ArrayBuffer
import collection.mutable.ListBuffer
import collection.mutable.Set

object Day15 {

    type Caves = Map[Point, Map[Point, Int]]

    case class Point(x: Int, y: Int) {
        override def toString() = "(" + x + "," + y + ")"
    }

    object Type extends Enumeration {
        val Goblin, Elf = Value
    }

    def main(args: Array[String]) {
        val lines = Problem.parseInputToList("day15")
        println(play(lines))
    }

    case class Character(pos: Point, side: Type.Value, hp: Int) {

        def attack(allChars: Array[Character], g: Grid) = {
            val targets = adjacentTargets(allChars, g).filter(_.hp > 0)

            if (targets.nonEmpty) {
                val grouped  = targets.groupBy(_.hp)
                val selected = grouped(grouped.keys.min)
                    .sortBy(ch => (ch.pos.y, ch.pos.x))
                    .head
                val index = allChars.indexWhere(ch => ch.pos == selected.pos)
                allChars(index) = allChars(index).takeDamage
            }
        }

        def takeDamage = copy(hp = hp - 3)

        def nextToTarget(allChars: Array[Character], g: Grid) =
            adjacentTargets(allChars, g).nonEmpty

        private def adjacentTargets(allChars: Array[Character], g: Grid) =
            allChars
                .filter(_.side != side)
                .filter(t =>
                    g.nonDiagNeighbours(t.pos.x, t.pos.y, false)
                     .filter(p => Point(p._1, p._2) == pos)
                     .nonEmpty
                )

        def move
            ( allChars: Array[Character]
            , g: Grid
            , graph: WeightedUndirectedGraph[Point]) = {

            val targets = allChars.filter(_.side != side)
            val allLocs = allChars.map(_.pos).filterNot(_ == pos)

            val candidateLocs = targets.flatMap(t =>
                g.nonDiagNeighbours(t.pos.x, t.pos.y, false)
                 .filter(p => g.get(p._1, p._2) == true)
                 .filterNot(p => allLocs.contains(Point(p._1, p._2)))
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

                val shortestPathsToCandidates = candidateLocs
                    .flatMap(l => shortestPathsTo(Point(l._1, l._2), usableGraph))

                /*
                    If there are no paths to candidate points, this means that
                    all paths are blocked by walls or other characters, so we
                    can't move
                */
                if (shortestPathsToCandidates.isEmpty) this
                else {
                    /*
                        All edge weights are 1 so we just need the shortes
                        path, not the shortest total weight (or rather, they
                        are equivalent and the former is simpler)
                    */
                    val groupedPaths  = shortestPathsToCandidates.groupBy(_.size)
                    val shortestPaths = groupedPaths(groupedPaths.keys.min)

                    val chosen = shortestPaths
                        .map(_.drop(1).head)
                        .sortBy(p => (p.y, p.x))
                        .head

                    copy(pos = chosen)
                }
            }
        }

        def shortestPathsTo
            ( target: Point
            , g: WeightedUndirectedGraph[Point])
            : List[List[Point]] = {

            val shortestPath =
                Dijkstra.shortestPath[Point](g, pos, target)

            if (!shortestPath.contains(pos)) return List[List[Point]]()

            val shortestPathLength = shortestPath.size
            val paths = ListBuffer[List[Point]]()

            def dfs
                ( point: Point
                , path: ListBuffer[Point]
                , visited: Set[Point]) {

                if (path.size > shortestPathLength) return

                if (point == target) {
                    paths += path.toList
                    return
                }

                visited += point

                for (p <- g.neighbours(point)) {
                    if (!visited.contains(p)) {
                        path += p
                        dfs(p, path, visited)
                        path -= p
                    }
                }

                visited -= point
            }

            dfs(pos, ListBuffer[Point](pos), Set[Point]())
            paths.toList
        }

        override def toString() =
            (if (side == Type.Goblin) "G" else "E") + "-" + pos.toString
    }

    def play(lines: List[String]) = {
        val (map, goblins, elves) = parseInput(lines)
        val graph = new WeightedUndirectedGraph(constructGraph(map))

        def turn(gs: Array[Character], es: Array[Character], rounds: Int): Int = {
            val all = (gs ++ es).sortBy(ch => (ch.pos.y, ch.pos.x))

            (0 until all.size).foreach(i => {
                val ch = all(i)

                val livingElves = filterSide(all, Type.Elf).filter(_.hp > 0)
                val livingGoblins = filterSide(all, Type.Goblin).filter(_.hp > 0)

                if (ch.side == Type.Goblin && livingElves.isEmpty)
                    return rounds * livingGoblins.map(_.hp).sum
                else if (ch.side == Type.Elf && livingGoblins.isEmpty)
                    return rounds * livingElves.map(_.hp).sum

                all(i) = ch.move(all, map, graph)
                ch.attack(all, map)
            })

            val alive = all.filter(_.hp > 0)
            val livingGoblins = filterSide(alive, Type.Goblin)
            val livingElves   = filterSide(alive, Type.Elf)

            turn(livingGoblins, livingElves, rounds + 1)
        }

        turn(goblins.toArray, elves.toArray, 0)
    }

    private def filterSide(chars: Array[Character], side: Type.Value) =
        chars.filter(_.side == side)

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

    def printMap(grid: Grid, chars: Array[Character]) {
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

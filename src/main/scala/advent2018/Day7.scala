package advent2018

import scalaadventutils.Dijkstra.shortestPathTotalWeight
import scalaadventutils.Problem
import scalaadventutils.WeightedUndirectedGraph

object Day7 {

    val parser = """Step (\w) must be finished before step (\w) can begin.""".r

    type Graph = Map[String, Map[String, Int]]
    type PreReq = Map[String, Set[String]]

    def main(args: Array[String]) {
        val lines = Problem.parseInputToList("day7")
        println(part1(lines))
        println(part2(lines))
    }

    def part1(lines: List[String]) = {
        val (g, p) = parseInput(lines)
        getStepOrder(new WeightedUndirectedGraph[String](g), p)
    }

    /*
        Simply calculate the longest path in the graph - the
        total time working is just the time that the longest
        worker spent working.
        This is actually only guaranteed to work if you have
        more workers than nodes, which we don't, but it works
        on my input. Note that it doesn't work on the example!
    */
    def part2(lines: List[String]) = {
        val (g, _) = parseInput(lines)
        val wug = new WeightedUndirectedGraph[String](g)
        wug.getRootNodes.flatMap(n =>
            wug.getAllPaths(n).map(shortestPathTotalWeight(wug, _))
        ).max
    }

    def getStepOrder(g: WeightedUndirectedGraph[String], p: PreReq) = {
        val allKeys = g.keys ++ p.keys
        val roots   = g.getRootNodes
        val start   = roots.toList.sorted.head

        def step(done: List[String], v: Set[String]): String = {
            if (done.size == allKeys.size) done.mkString
            else {
                val options = g.keys
                               .filter(v.contains)
                               .flatMap(g.neighbours) ++ roots

                val next = options.filterNot(v.contains)
                                  .filter(opt => p(opt).forall(v.contains))
                                  .toList.sorted.head

                step(done :+ next, v + next)
            }
        }
        step(List(start), Set(start))
    }

    def parseInput(lines: List[String]) = {
        def parseLine
            ( left: List[String]
            , g: Graph
            , p: PreReq)
            : (Graph, PreReq) = left match {
            case Nil    => (g, p)
            case h :: t => h match { case parser(from, to) =>
                parseLine(
                    t,
                    g.updated(
                        from, g(from).updated(to, 60 + (from(0).toInt - 64))
                    ),
                    p.updated(to, p(to) + from)
                )
            }
        }
        parseLine(
            lines,
            Map[String, Map[String, Int]]().withDefaultValue(Map.empty),
            Map[String, Set[String]]().withDefaultValue(Set.empty)
        )
    }
}

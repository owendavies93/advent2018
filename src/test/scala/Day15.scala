package advent2018

import scalaadventutils.Problem
import scalaadventutils.WeightedUndirectedGraph

import org.scalatest.funsuite.AnyFunSuite

class Day15Spec extends AnyFunSuite {
    val l1 = Problem.parseInputToList("day15-test1")
    val l2 = Problem.parseInputToList("day15-test2")
    val l3 = Problem.parseInputToList("day15-test3")

    test("Day 15: Character.move example 1") {
        val (map, goblins, elves) = Day15.parseInput(l1)
        val graph = new WeightedUndirectedGraph(Day15.constructGraph(map))

        val testElf = elves.head
        val all = goblins ++ elves

        assert(!testElf.nextToTarget(all, map))

        assertResult(Day15.Point(2, 1)) {
            testElf.move(all, map, graph).pos
        }
    }

    test("Day 15: Character.move example 2") {
        val (map, goblins, elves) = Day15.parseInput(l2)
        val graph = new WeightedUndirectedGraph(Day15.constructGraph(map))

        val testElf = elves.head
        val all = goblins ++ elves

        assert(!testElf.nextToTarget(all, map))

        assertResult(Day15.Point(3, 1)) {
            testElf.move(all, map, graph).pos
        }
    }

    test("Day 15: Character.move example 3") {
        val (map, goblins, elves) = Day15.parseInput(l3)
        val graph = new WeightedUndirectedGraph(Day15.constructGraph(map))

        val all = readingOrder(goblins ++ elves)

        Day15.printMap(map, all)

        var next = readingOrder(all.map(_.move(all, map, graph)))

        var elf = next.filter(_.side == Day15.Type.Elf).head

        Day15.printMap(map, next)

        assert(elf.nextToTarget(next, map))

        assertResult(Day15.Point(4,3)) {
            elf.pos
        }

        next = readingOrder(next.map(_.move(next, map, graph)))

        Day15.printMap(map, next)

        elf = next.filter(_.side == Day15.Type.Elf).head

        assert(elf.nextToTarget(next, map))

        assertResult(Day15.Point(4,3)) {
            elf.pos
        }

        next = readingOrder(next.map(_.move(next, map, graph)))

        Day15.printMap(map, next)
    }

    def readingOrder(l: List[Day15.Character]) =
        l.sortBy(ch => (ch.pos.y, ch.pos.x))
}

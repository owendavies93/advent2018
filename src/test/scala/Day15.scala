package advent2018

import scalaadventutils.Problem
import scalaadventutils.WeightedUndirectedGraph

import org.scalatest.funsuite.AnyFunSuite

class Day15Spec extends AnyFunSuite {
    val l1 = Problem.parseInputToList("day15-test1")
    val l2 = Problem.parseInputToList("day15-test2")
    val l3 = Problem.parseInputToList("day15-test3")
    val l4 = Problem.parseInputToList("day15-test4")

    test("Day 15: Character.move example 1") {
        val (map, goblins, elves) = Day15.parseInput(l1)
        val graph = new WeightedUndirectedGraph(Day15.constructGraph(map))

        val testElf = elves.head
        val all = (goblins ++ elves).toArray

        assert(!testElf.nextToTarget(all, map))

        assertResult(Day15.Point(2, 1)) {
            testElf.move(all, map, graph).pos
        }
    }

    test("Day 15: Character.move example 2") {
        val (map, goblins, elves) = Day15.parseInput(l2)
        val graph = new WeightedUndirectedGraph(Day15.constructGraph(map))

        val testElf = elves.head
        val all = (goblins ++ elves).toArray

        assert(!testElf.nextToTarget(all, map))

        assertResult(Day15.Point(3, 1)) {
            testElf.move(all, map, graph).pos
        }
    }

    test("Day 15: Character.move example 3") {
        val (map, goblins, elves) = Day15.parseInput(l3)
        val graph = new WeightedUndirectedGraph(Day15.constructGraph(map))

        val all = readingOrder(goblins ++ elves)

        (0 until all.size).foreach(i => all(i) = all(i).move(all, map, graph))

        var elf = all.filter(_.side == Day15.Type.Elf).head

        assert(elf.nextToTarget(all, map))

        assertResult(Day15.Point(4,3)) {
            elf.pos
        }

        (0 until all.size).foreach(i => all(i) = all(i).move(all, map, graph))

        elf = all.filter(_.side == Day15.Type.Elf).head

        assert(elf.nextToTarget(all, map))

        assertResult(Day15.Point(4,3)) {
            elf.pos
        }

        (0 until all.size).foreach(i => all(i) = all(i).move(all, map, graph))
    }

    test("Day 15: play example 1") {
        Day15.play(l4)
    }

    def readingOrder(l: List[Day15.Character]) =
        l.sortBy(ch => (ch.pos.y, ch.pos.x)).toArray
}

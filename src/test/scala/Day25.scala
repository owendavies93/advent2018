package advent2018

import scalaadventutils.Problem

import org.scalatest.funsuite.AnyFunSuite

class Day25Spec extends AnyFunSuite {

    test("Day 25: part1") {
        val l1 = Problem.parseInputToList("day25-test1")

        assertResult(2) {
            Day25.part1(l1)
        }

        val l2 = Problem.parseInputToList("day25-test2")

        assertResult(4) {
            Day25.part1(l2)
        }

        val l3 = Problem.parseInputToList("day25-test3")

        assertResult(3) {
            Day25.part1(l3)
        }

        val l4 = Problem.parseInputToList("day25-test4")

        assertResult(8) {
            Day25.part1(l4)
        }
    }
}

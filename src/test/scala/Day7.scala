package advent2018

import scalaadventutils.Problem

import org.scalatest.funsuite.AnyFunSuite

class Day7Spec extends AnyFunSuite {
    val lines = Problem.parseInputToList("day7-test")

    test("Day 7: part1") {
        assertResult("CABDFE") {
            Day7.part1(lines)
        }
    }

    test("Day 7: part2") {
        Day7.part2(lines)
    }
}

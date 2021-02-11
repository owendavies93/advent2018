package advent2018

import scalaadventutils.Problem

import org.scalatest.funsuite.AnyFunSuite

class Day13Spec extends AnyFunSuite {

    test("Day 13: part1") {
        val lines = Problem.parseInputToList("day13-test")
        assertResult(Day13.Point(7,3)) {
            Day13.part1(lines)
        }
    }

    test("Day 13: part2") {
        val lines = Problem.parseInputToList("day13-test2")
        assertResult(Day13.Point(6,4)) {
            Day13.part2(lines)
        }
    }
}

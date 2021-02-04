package advent2018

import scalaadventutils.Problem

import org.scalatest.funsuite.AnyFunSuite

class Day3Spec extends AnyFunSuite {

    test("Day 3: part1") {
        val lines = Problem.parseInputToList("day3-test")

        assertResult(0) {
            Day3.part1(List("#123 @ 3,2: 5x4"))
        }

        assertResult(4) {
            Day3.part1(lines)
        }
    }

    test("Day 3: part2") {
        val lines = Problem.parseInputToList("day3-test")

        assertResult(3) {
            Day3.part2(lines)
        }
    }
}

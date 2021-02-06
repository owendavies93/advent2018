package advent2018

import scalaadventutils.Problem

import org.scalatest.funsuite.AnyFunSuite

class Day6Spec extends AnyFunSuite {

    val lines = Problem.parseInputToList("day6-test")

    test("Day 6: findLargestArea") {
        assertResult(17) {
            Day6.findLargestArea(lines)
        }
    }

    test("Day 6: findPointsBelowDistance") {
        assertResult(16) {
            Day6.findPointsBelowDistance(lines, 32)
        }
    }
}

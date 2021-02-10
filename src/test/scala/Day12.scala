package advent2018

import scalaadventutils.Problem

import org.scalatest.funsuite.AnyFunSuite

class Day12Spec extends AnyFunSuite {

    test("Day 12: run") {
        val lines = Problem.parseInputToList("day12-test")

        assertResult(325) {
            Day12.run(lines, 20)
        }
    }
}

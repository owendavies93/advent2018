package advent2018

import scalaadventutils.Problem

import org.scalatest.funsuite.AnyFunSuite

class Day19Spec extends AnyFunSuite {

    test("Day 19: run") {
        val lines = Problem.parseInputToList("day19-test")
        assertResult(7) {
            Day19.run(0, lines)
        }
    }
}

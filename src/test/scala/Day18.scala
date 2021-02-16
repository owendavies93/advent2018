package advent2018

import scalaadventutils.Problem

import org.scalatest.funsuite.AnyFunSuite

class Day18Spec extends AnyFunSuite {

    test("Day 18: step") {
        val lines = Problem.parseInputToList("day18-test")

        assertResult(1147) {
            Day18.step(lines, 10)
        }
    }
}

package advent2018

import scalaadventutils.Problem

import org.scalatest.funsuite.AnyFunSuite

class Day18Spec extends AnyFunSuite {

    test("Day 18: step") {
        val lines = Problem.parseInputToList("day18-test")
        val ca = Day18.parseInput(lines)

        assertResult(1147) {
            Day18.step(ca, 10)
        }
    }
}

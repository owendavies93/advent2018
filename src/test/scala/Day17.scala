package advent2018

import scalaadventutils.Problem

import org.scalatest.funsuite.AnyFunSuite

class Day17Spec extends AnyFunSuite {

    test("Day 17: countSquares") {
        val lines = Problem.parseInputToList("day17-test")
        val grid  = Day17.parseInput(lines)

        assertResult(57) {
            Day17.countSquares(grid)
        }

        assertResult(29) {
            Day17.countSquares(grid, true)
        }
    }
}

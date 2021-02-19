package advent2018

import scalaadventutils.Problem

import org.scalatest.funsuite.AnyFunSuite

class Day23Spec extends AnyFunSuite {

    test("Day 23: part1") {
        val lines = Problem.parseInputToList("day23-test")

        assertResult(7) {
            Day23.part1(lines)
        }
    }
}

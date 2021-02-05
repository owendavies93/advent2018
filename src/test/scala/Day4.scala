package advent2018

import scalaadventutils.Problem

import org.scalatest.funsuite.AnyFunSuite

class Day4Spec extends AnyFunSuite {

    test("Day 4: part1") {
        val lines = Problem.parseInputToList("day4-test")

        assertResult(240) {
            Day4.part1(lines)
        }
    }

    test("Day 4: part2") {
        val lines = Problem.parseInputToList("day4-test")

        assertResult(4455) {
            Day4.part2(lines)
        }
    }
}

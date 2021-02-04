package advent2018

import scalaadventutils.Problem

import org.scalatest.funsuite.AnyFunSuite

class Day2Spec extends AnyFunSuite {

    test("Day 2: countOcc") {
        assert(Day2.countOcc(List("abcccd"), 3) == 1)
        assert(Day2.countOcc(List("abcccd"), 2) == 0)
    }

    test("Day 2: part2") {
        val lines = Problem.parseInputToList("day2-test")

        assertResult("fgij") {
            Day2.part2(lines)
        }
    }
}

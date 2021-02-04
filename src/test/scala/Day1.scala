package advent2018

import org.scalatest.funsuite.AnyFunSuite

class Day1Spec extends AnyFunSuite {

    test("Day 1: part1") {
        assertResult(0) {
            Day1.part1(List("+1", "+1", "-2"))
        }
    }

    test("Day 1: part2") {
        assertResult(10) {
            Day1.part2(List("+3", "+3", "+4", "-2", "-4"))
        }

        assertResult(5) {
            Day1.part2(List("-6", "+3", "+8", "+5", "-6"))
        }
    }
}

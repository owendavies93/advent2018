package advent2018

import org.scalatest.funsuite.AnyFunSuite

class Day22Spec extends AnyFunSuite {

    test("Day 22: part1") {
        assertResult(114) {
            Day22.part1(Day22.Point(10, 10), 510)
        }
    }
}

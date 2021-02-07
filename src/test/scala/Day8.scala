package advent2018

import org.scalatest.funsuite.AnyFunSuite

class Day8Spec extends AnyFunSuite {
    val values = List(2, 3, 0, 3, 10, 11, 12, 1, 1, 0, 1, 99, 2, 1, 1, 2)

    test("Day 8: sumMeta") {
        assertResult(138) {
            Day8.sumMeta(values)
        }
    }

    test("Day 8: findValue") {
        assertResult(66) {
            Day8.findValue(values)
        }
    }
}

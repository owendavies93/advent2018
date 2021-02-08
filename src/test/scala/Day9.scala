package advent2018

import org.scalatest.funsuite.AnyFunSuite

class Day9Spec extends AnyFunSuite {

    test("Day 9: play") {
        assertResult(32) {
            Day9.play(9, 25)
        }

        assertResult(8317) {
            Day9.play(10, 1618)
        }

        assertResult(146373) {
            Day9.play(13, 7999)
        }
    }
}

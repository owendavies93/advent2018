package advent2018

import org.scalatest.funsuite.AnyFunSuite

class Day11Spec extends AnyFunSuite {

    test("Day 11: powerLevel") {
        assert(Day11.powerLevel(3, 5, 8) == 4)
        assert(Day11.powerLevel(122, 79, 57) == -5)
        assert(Day11.powerLevel(217, 196, 39) == 0)
        assert(Day11.powerLevel(101, 153, 71) == 4)
    }

    test("Day 11: part1") {
        assertResult((33, 45)) {
            Day11.part1(18)
        }

        assertResult((21, 61)) {
            Day11.part1(42)
        }
    }

    test("Day 11: part2") {
        assertResult((90,269,16)) {
            Day11.part2(18)
        }

        assertResult((232,251,12)) {
            Day11.part2(42)
        }
    }
}

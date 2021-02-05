package advent2018

import org.scalatest.funsuite.AnyFunSuite

class Day5Spec extends AnyFunSuite {

    test("Day 5: react") {
        assert(Day5.react("dabAcCaCBAcCcaDA") == "dabAaCBAcCcaDA")
        assert(Day5.react("dabCBAcCcaDA") == "dabCBAcaDA")
        assert(Day5.react("dabCBAcaDA") == "dabCBAcaDA")
    }

    test("Day 5: runReactions") {
        assert(Day5.runReactions("dabAcCaCBAcCcaDA") == 10)
    }

    test("Day 5: remove") {
        assert(Day5.remove("dabAcCaCBAcCcaDA") == 4)
    }
}

package advent2018

import scalaadventutils.Problem

import org.scalatest.funsuite.AnyFunSuite

class Day24Spec extends AnyFunSuite {

    test("Day 24: fight") {
        val immune    = Problem.parseInputToList("day24-test-immune")
        val infection = Problem.parseInputToList("day24-test-infection")

        var im = Day24.parseInput(immune, Day24.Immune, 0)
        val in = Day24.parseInput(infection, Day24.Infection, 0)

        assertResult(5216) {
            Day24.fight(in, im)
        }

        im = Day24.parseInput(immune, Day24.Immune, 1570)

        assertResult(51) {
            Day24.fight(in, im)
        }
    }
}

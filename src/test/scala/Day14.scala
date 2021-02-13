package advent2018

import org.scalatest.funsuite.AnyFunSuite

class Day14Spec extends AnyFunSuite {

    test("Day 14: run") {
        assertResult("5158916779") {
            Day14.run(9)
        }

        assertResult("0124515891") {
            Day14.run(5)
        }

        assertResult("9251071085") {
            Day14.run(18)
        }

        assertResult("5941429882") {
            Day14.run(2018)
        }
    }

    test("Day 14: recipeCount") {
        assertResult(9) {
            Day14.recipeCount("51589")
        }

        assertResult(5) {
            Day14.recipeCount("01245")
        }

        assertResult(18) {
            Day14.recipeCount("92510")
        }

        assertResult(2018) {
            Day14.recipeCount("59414")
        }
    }
}

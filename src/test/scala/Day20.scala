package advent2018

import scalaadventutils.Problem

import org.scalatest.funsuite.AnyFunSuite

class Day20Spec extends AnyFunSuite {
    test("Day 20: findFurthestRoom") {
        val l1 = Problem.parseInputToString("day20-test1")

        assertResult(10) {
            Day20.findFurthestRoom(l1)
        }

        val l2 = Problem.parseInputToString("day20-test2")

        assertResult(18) {
            Day20.findFurthestRoom(l2)
        }

        val l3 = Problem.parseInputToString("day20-test3")

        assertResult(23) {
            Day20.findFurthestRoom(l3)
        }

        val l4 = Problem.parseInputToString("day20-test4")

        assertResult(31) {
            Day20.findFurthestRoom(l4)
        }
    }
}

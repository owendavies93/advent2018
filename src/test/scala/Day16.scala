package advent2018

import scalaadventutils.Problem

import org.scalatest.funsuite.AnyFunSuite

class Day16Spec extends AnyFunSuite {

    test("Day 16: parseInput") {
        val lines = Problem.parseInputToList("day16-test")
        assert(Day16.part1(lines) == 1)
    }
}

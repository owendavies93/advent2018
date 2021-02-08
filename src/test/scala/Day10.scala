package advent2018

import scalaadventutils.Problem

import org.scalatest.funsuite.AnyFunSuite

class Day10Spec extends AnyFunSuite {
    test("Day 10: step") {
        val lines = Problem.parseInputToList("day10-test")
        Day10.step(4, lines)
    }
}

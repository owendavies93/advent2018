package advent2018

import scalaadventutils.Profiler.timeMS

import collection.mutable.ArrayBuffer

object Day11 {

    def main(args: Array[String]) {
        println(part1(5468))
        println(part2(5468))
    }

    def part1(serial: Int, dim: Int = 300) =
        Grid(dim, dim, serial).maxPower(3)._1

    def part2(serial: Int, dim: Int = 300) =
        Grid(dim, dim, serial).maxPowerAllSizes

    def powerLevel(x: Int, y: Int, serial: Int): Int = {
        val rack = x + 10
        val power = (rack * y + serial) * rack
        ((power / 100) % 10) - 5
    }

    object Grid {
        def apply(height: Int, width: Int, serial: Int): Grid = {
            val arr =
                (0 until height).flatMap(y =>
                    (0 until width).map(x =>
                        powerLevel(x, y, serial)
                    )
                ).toArray

            Grid(arr, width, height)
        }
    }

    case class Grid(grid: Array[Int], width: Int, height: Int) {
        def get(x: Int, y: Int) = grid(y * width + x)

        def squarePower(x: Int, y: Int, size: Int) =
            (y until y + size).flatMap(y_ =>
                (x until x + size).map(x_ => get(x_, y_))
            ).sum

        /*
            Attempts to minimise calculation by only considering
            positive starting points
        */
        def maxPower(size: Int) =
            (0 until height - size).flatMap(y =>
                (0 until width - size).map(x =>
                    if (get(x, y) < 0) (x, y) -> Int.MinValue
                    else (x, y) -> squarePower(x, y, size)
                )
            ).maxBy(_._2)

        /*
            Only iterate as far as we need. In reality I kept it
            running until the max size started to fall
        */
        def maxPowerAllSizes = {
            val res = (0 to 16).map(i => (i -> maxPower(i))).maxBy(_._2._2)
            (res._2._1._1, res._2._1._2, res._1)
        }
    }
}

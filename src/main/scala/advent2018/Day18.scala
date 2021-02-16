package advent2018

import scalaadventutils.Problem

object Day18 {

    def main(args: Array[String]): Unit = {
        val lines = Problem.parseInputToList("day18")
        val ca = parseInput(lines)
        println(step(ca, 10))
        println(step(ca, 500))
    }

    sealed trait Square

    case object Open extends Square {
        override def toString() = "."
    }

    case object Lumber extends Square {
        override def toString() = "#"
    }

    case object Tree extends Square {
        override def toString() = "|"
    }

    def step(ca: CA, times: Int): Int = {
        val seen = Map[Int, (CA, Int)]()
        val res = (1 to times).foldLeft((ca, seen))((n, i) => {
            val (next, s) = n
            def stepFn(x: Int, y: Int): Square = {
                val ns = next.neighbours(x, y).map(p => next.get(p._1, p._2))
                next.get(x, y) match {
                    case Open   => if (ns.count(_ == Tree) >= 3) Tree else Open
                    case Tree   => if (ns.count(_ == Lumber) >= 3) Lumber else Tree
                    case Lumber =>
                        if (ns.count(_ == Lumber) >= 1 && ns.count(_ == Tree) >= 1)
                            Lumber
                        else Open
                }
            }

            val newCA = next.step(stepFn)

            val hash = newCA.##
            if (s.contains(hash)) {
                val cycleLength = i - s(hash)._2
                val res = s(hash)._2 + (1000000000 - i) % cycleLength
                val ca = s.find(_._2._2 == res).get._2._1
                println(ca.grid.count(_ == Tree) * ca.grid.count(_ == Lumber))
            }

            (newCA, s.updated(hash, (newCA, i)))
        })._1
        res.grid.count(_ == Tree) * res.grid.count(_ == Lumber)
    }

    def parseInput(arr: List[String]): CA = {
        val height = arr.size
        val width  = arr(0).size

        val grid: Array[Square] = (0 until height).flatMap(y =>
            (0 until width).map(x => arr(y)(x) match {
                case '.' => Open
                case '#' => Lumber
                case '|' => Tree
            })
        ).toArray

        CA(grid, width, height)
    }

    /*
        TODO: make the util CA and Grid classes generic, such that it can take
              a sealed trait as a type instead of just a Boolean
    */
    case class CA(grid: Array[Square], width: Int, height: Int) {
        private val neighbourList = List(
            (-1, -1), (-1, 0), (0, -1), (1, -1),
            (1, 1), (-1, 1), (1, 0), (0, 1)
        )

        def checkBounds(x: Int, y: Int): Boolean = {
            val xMatch = x match {
                case i if 0 until width contains x => true
                case _                             => false
            }

            val yMatch = y match {
                case i if 0 until height contains i => true
                case _                              => false
            }

            xMatch && yMatch
        }

        def get(x: Int, y: Int) = grid(y * width + x)

        def neighbours(x: Int, y: Int) =
            neighbourList.map(n => (x + n._1, y + n._2))
                         .filter(n => checkBounds(n._1, n._2))

        def step(stepFn: (Int, Int) => Square): CA = {
            val nextGrid = (0 until height).flatMap(y =>
                (0 until width).map(x =>
                    stepFn(x, y)
                )
            ).toArray

            CA(nextGrid, width, height)
        }

        override def toString(): String = {
            val sb = new StringBuilder

            for (y <- 0 until height) {
                for (x <- 0 until width) {
                    sb.append(get(x, y))
                }

                if (y < width - 1) {
                    sb.append("\n")
                }
            }

            sb.toString()
        }

        override def hashCode() = grid.toSeq.hashCode()
    }
}

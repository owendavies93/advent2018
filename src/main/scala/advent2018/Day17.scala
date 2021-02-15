package advent2018

import scalaadventutils.Problem

object Day17 {

    val parseX = """x=(\d+), y=(\d+)..(\d+)""".r
    val parseY = """y=(\d+), x=(\d+)..(\d+)""".r

    def main(args: Array[String]): Unit = {
        val lines = Problem.parseInputToList("day17")
        val squares = parseInput(lines)
        println(countSquares(squares))
        println(countSquares(squares, true))
    }

    def countSquares(squares: Grid, stopped: Boolean = false): Int = {
        val (minx, maxx, miny, maxy) = getBounds(squares)

        def run(g: Grid, curr: Point, prev: Point): Grid = {
            if (curr.y > maxy) g
            else {
                g(curr) match {
                    case Sand | Spring => {
                        val d = curr.down
                        val dSq = run(g + (curr -> MovingWater), d, curr)
                        dSq(d) match {
                            case Clay | RestingWater => {
                                val l = curr.left
                                val r = curr.right
                                val lSq = run(dSq, l, curr)
                                val rSq = run(lSq, r, curr)
                                (rSq(l), rSq(r)) match {
                                    case (Clay | RestingWater | SettlingWater, _) |
                                         (_, Clay | RestingWater | SettlingWater)
                                    if (prev == l || prev == r) =>
                                        rSq + (curr -> SettlingWater)
                                    case (Clay | RestingWater | SettlingWater,
                                          Clay | RestingWater | SettlingWater) =>
                                        settle(settle(rSq, l), r) +
                                        (curr -> RestingWater)
                                    case _ => rSq
                                }
                            }
                            case MovingWater | SettlingWater | Sand | Spring => dSq
                        }
                    }
                    case Clay | MovingWater | RestingWater | SettlingWater => g
                }
            }
        }

        val springSquare = Point(500, 0)
        val result = run(squares + (springSquare -> Spring),
                         springSquare, springSquare.up)

        result.count({
            case (p, sq) =>
                p.y >= miny &&
                (if (stopped) sq == RestingWater else sq.isInstanceOf[Water])
        })
    }

    private def settle(g: Grid, p: Point): Grid = g(p) match {
        case SettlingWater =>
            settle(settle(g + (p -> RestingWater), p.left), p.right)
        case _ => g
    }

    def parseInput(lines: List[String]): Grid =
        lines.flatMap(l => l match {
            case parseX(x, y, y_) =>
                (y.toInt to y_.toInt).map(Point(x.toInt, _) -> Clay)
            case parseY(y, x, x_) =>
                (x.toInt to x_.toInt).map(Point(_, y.toInt) -> Clay)
        }).toMap.withDefaultValue(Sand)

    def printGrid(g: Grid): Unit = {
        val (minx, maxx, miny, maxy) = getBounds(g)
        val out =
            (miny to maxy).map(y =>
                (minx to maxx).map(x => g(Point(x, y))).mkString
            ).mkString("\n")
        println(out)
    }

    private def getBounds(g: Grid): (Int, Int, Int, Int) = {
        val keys = g.keys
        val minx = keys.minBy(_.x).x
        val maxx = keys.maxBy(_.x).x
        val miny = keys.minBy(_.y).y
        val maxy = keys.maxBy(_.y).y
        (minx, maxx, miny, maxy)
    }

    sealed trait Square

    case object Clay extends Square {
        override def toString() = "#"
    }

    case object Sand extends Square {
        override def toString() = "."
    }

    case object Spring extends Square {
        override def toString() = "+"
    }

    sealed trait Water extends Square

    case object MovingWater extends Water {
        override def toString() = "|"
    }

    case object RestingWater extends Water {
        override def toString() = "~"
    }

    case object SettlingWater extends Water {
        override def toString() = "/"
    }

    case class Point(x: Int, y: Int) {
        def left  = copy(x = x - 1)
        def right = copy(x = x + 1)
        def down  = copy(y = y + 1)
        def up    = copy(y = y - 1)
    }

    type Grid = Map[Point, Square]

}

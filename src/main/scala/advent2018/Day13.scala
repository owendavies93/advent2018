package advent2018

import scalaadventutils.Problem

object Day13 {

    def main(args: Array[String]) {
        val lines = Problem.parseInputToList("day13")
        println(part1(lines))
        println(part2(lines))
    }

    def part1(lines: List[String]) = {
        val (grid, carts) = parseInput(lines)
        findCollision(grid, carts)
    }

    def part2(lines: List[String]) = {
        val (grid, carts) = parseInput(lines)
        findLastCart(grid, carts)
    }

    def findLastCart(grid: Grid, carts: List[Cart]) = {
        def find_(cs: List[Cart]): Cart = {
            val nextCarts = removeCollidingCarts(cs)
            if (nextCarts.size == 1) nextCarts(0)
            else
                find_(nextCarts.sortBy(_.loc.x).map(_.move(grid)))
        }

        find_(carts).loc
    }

    def findCollision(grid: Grid, carts: List[Cart]) = {
        def find_(cs: List[Cart]): Point = {
            val col = checkCollisions(cs)
            if (col.isDefined) col.get
            else
                find_(cs.sortBy(_.loc.x).map(_.move(grid)))
        }

        find_(carts)
    }

    def checkCollisions(cs: List[Cart]): Option[Point] =
        cs.groupBy(_.loc).find(_._2.size > 1).map(_._1)

    def removeCollidingCarts(cs: List[Cart]): List[Cart] =
        cs.groupBy(_.loc).filter(_._2.size == 1).values.flatten.toList

    def parseInput(lines: List[String]): (Grid, List[Cart]) = {
        val height = lines.size
        val width  = lines(0).size

        val grid = Grid(
            (0 until height).flatMap(y =>
                (0 until width).map(x => lines(y)(x))
            ).toArray, height, width)

        val carts = grid.findCarts

        (grid, carts)
    }

    object Direction extends Enumeration {
        val U, D, L, R = Value
    }

    object Turn extends Enumeration {
        val Left, Straight1, Right, Straight2 = Value
    }

    case class Point(x: Int, y: Int) {
        import Day13.Direction._

        def next(d: Direction.Value) = d match {
            case U => copy(y = y - 1)
            case D => copy(y = y + 1)
            case L => copy(x = x - 1)
            case R => copy(x = x + 1)
        }
    }

    case class Cart(loc: Point, dir: Direction.Value, nextTurn: Turn.Value) {
        import Day13.Direction._
        import Day13.Turn._

        def turnAtIntersection = nextTurn match {
            case Left => dir match {
                case U => copy(dir = L, nextTurn = Straight1)
                case D => copy(dir = R, nextTurn = Straight1)
                case L => copy(dir = D, nextTurn = Straight1)
                case R => copy(dir = U, nextTurn = Straight1)
            }
            case Right => dir match {
                case U => copy(dir = R, nextTurn = Straight2)
                case D => copy(dir = L, nextTurn = Straight2)
                case L => copy(dir = U, nextTurn = Straight2)
                case R => copy(dir = D, nextTurn = Straight2)
            }
            case Straight1 => copy(nextTurn = Right)
            case Straight2 => copy(nextTurn = Left)
        }

        def move(g: Grid) = {
            val p = loc.next(dir)
            g.get(p) match {
                case '\\' => dir match {
                    case U => copy(dir = L, loc = p)
                    case D => copy(dir = R, loc = p)
                    case L => copy(dir = U, loc = p)
                    case R => copy(dir = D, loc = p)
                }
                case '/' => dir match {
                    case U => copy(dir = R, loc = p)
                    case D => copy(dir = L, loc = p)
                    case L => copy(dir = D, loc = p)
                    case R => copy(dir = U, loc = p)
                }
                case '+' => copy(loc = p).turnAtIntersection
                case _   => copy(loc = p)
            }
        }
    }

    case class Grid(grid: Array[Char], height: Int, width: Int) {
        import Day13.Direction._
        import Day13.Turn._

        def get(p: Point): Char = grid(p.y * width + p.x)

        def cartPoints =
            (0 until height).flatMap(y =>
                (0 until width).map(x => Point(x, y)))
            .filter(p => {
                val ch = get(p)
                ch == '>' || ch == '<' || ch == 'v' || ch == '^'
            })

        def findCarts = cartPoints.map(cp => {
                val ch = get(cp)
                val dir = ch match {
                    case '>' => R
                    case '<' => L
                    case 'v' => D
                    case '^' => U
                }
                Cart(cp, dir, Left)
            }).toList

        def removeCarts = {
            val emptyGrid = grid.map(ch => ch match {
                case '>' => '-'
                case '<' => '-'
                case 'v' => '|'
                case '^' => '|'
                case c   => c
            }).toArray
            copy(grid = emptyGrid)
        }

        override def toString() =
            grid.grouped(width).map(_.mkString).mkString("\n")
    }
}

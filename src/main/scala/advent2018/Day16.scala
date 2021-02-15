package advent2018

import scalaadventutils.Problem

import annotation.tailrec

object Day16 {

    type Regs = Map[Int, Int]
    type Test = (Regs, List[Int], Regs)

    val commands = Set[Comm](
        Addr(), Addi(), Mulr(), Muli(), Banr(), Bani(), Borr(), Bori(),
        Setr(), Seti(), Gtir(), Gtri(), Gtrr(), Eqir(), Eqri(), Eqrr())

    def main(args: Array[String]): Unit = {
        val lines = Problem.parseInputToList("day16")
        println(part1(lines))
        println(part2(lines))
    }

    def part1(lines: List[String]) = {
        val (tests, _) = parseInput(lines)
        tests.count(t => countMatches(t, commands) >= 3)
    }

    def part2(lines: List[String]) = {
        val (tests, program) = parseInput(lines)

        @tailrec
        def findCommands(comms: Set[Comm], opCodes: Map[Int, Comm]): Map[Int, Comm] = {
            if (comms.isEmpty) opCodes
            else {
                val singleComms = tests.map(t => {
                    val (_, args, _) = t
                    (getMatchingComms(t, comms), args(0))
                }).filter(c => c._1.size == 1).distinct

                val toAdd = singleComms.map(c => c._2 -> c._1.head).toMap
                val toRemove = singleComms.map(_._1.head)

                findCommands(comms -- toRemove, opCodes ++ toAdd)
            }
        }

        val opMap = findCommands(commands, Map.empty)

        val init = Map[Int, Int]().withDefaultValue(0)
        val output = program.foldLeft(init)((regs, inst) => {
            val comm = opMap(inst(0))
            comm.run(inst(1), inst(2), inst(3), regs)
        })
        output(0)
    }

    def countMatches(t: Test, comms: Set[Comm]) = {
        val (before, args, after) = t
        comms.count(c => c.run(args(1), args(2), args(3), before) == after)
    }

    def getMatchingComms(t: Test, comms: Set[Comm]) = {
        val (before, args, after) = t
        comms.filter(c => c.run(args(1), args(2), args(3), before) == after)
    }

    def parseInput(lines: List[String]) = {
        def parseTests
            ( l: List[String]
            , ts: List[Test])
            : (List[Test], List[String]) = l match {
            case head :: tail if head == "" => (ts, tail)
            case List()                     => (ts, List.empty)
            case b :: args :: a :: "" :: tail => {
                val test = (getRegsFromLine(b), args.split(" ").map(_.toInt).toList, getRegsFromLine(a))
                parseTests(tail, test :: ts)
            }
            case _ => (ts, List.empty)
        }

        val (tests, rest) = parseTests(lines, List[Test]())
        val program = rest.filterNot(_ == "").map(_.split(" ").map(_.toInt))
        (tests, program)
    }

    val regsParser = """^(?:Before|After):\s+\[(\d), (\d), (\d), (\d)\]$""".r

    private def getRegsFromLine(line: String) = line match {
        case regsParser(one, two, three, four) => Map(
            0 -> one.toInt, 1 -> two.toInt, 2 -> three.toInt, 3 -> four.toInt
        )
    }

    abstract class Comm {
        def run(a: Int, b: Int, c: Int, regs: Regs): Regs
    }

    case class Addr() extends Comm {
        def run(a: Int, b: Int, c: Int, regs: Regs) =
            regs.updated(c, regs(a) + regs(b))
    }

    case class Addi() extends Comm {
        def run(a: Int, b: Int, c: Int, regs: Regs) =
            regs.updated(c, regs(a) + b)
    }

    case class Mulr() extends Comm {
        def run(a: Int, b: Int, c: Int, regs: Regs) =
            regs.updated(c, regs(a) * regs(b))
    }

    case class Muli() extends Comm {
        def run(a: Int, b: Int, c: Int, regs: Regs) =
            regs.updated(c, regs(a) * b)
    }

    case class Banr() extends Comm {
        def run(a: Int, b: Int, c: Int, regs: Regs) =
            regs.updated(c, regs(a) & regs(b))
    }

    case class Bani() extends Comm {
        def run(a: Int, b: Int, c: Int, regs: Regs) =
            regs.updated(c, regs(a) & b)
    }

    case class Borr() extends Comm {
        def run(a: Int, b: Int, c: Int, regs: Regs) =
            regs.updated(c, regs(a) | regs(b))
    }

    case class Bori() extends Comm {
        def run(a: Int, b: Int, c: Int, regs: Regs) =
            regs.updated(c, regs(a) | b)
    }

    case class Setr() extends Comm {
        def run(a: Int, b: Int, c: Int, regs: Regs) =
            regs.updated(c, regs(a))
    }

    case class Seti() extends Comm {
        def run(a: Int, b: Int, c: Int, regs: Regs) =
            regs.updated(c, a)
    }

    case class Gtir() extends Comm {
        def run(a: Int, b: Int, c: Int, regs: Regs) =
            regs.updated(c, if (a > regs(b)) 1 else 0)
    }

    case class Gtri() extends Comm {
        def run(a: Int, b: Int, c: Int, regs: Regs) =
            regs.updated(c, if (regs(a) > b) 1 else 0)
    }

    case class Gtrr() extends Comm {
        def run(a: Int, b: Int, c: Int, regs: Regs) =
            regs.updated(c, if (regs(a) > regs(b)) 1 else 0)
    }

    case class Eqir() extends Comm {
        def run(a: Int, b: Int, c: Int, regs: Regs) =
            regs.updated(c, if (a == regs(b)) 1 else 0)
    }

    case class Eqri() extends Comm {
        def run(a: Int, b: Int, c: Int, regs: Regs) =
            regs.updated(c, if (regs(a) == b) 1 else 0)
    }

    case class Eqrr() extends Comm {
        def run(a: Int, b: Int, c: Int, regs: Regs) =
            regs.updated(c, if (regs(a) == regs(b)) 1 else 0)
    }
}

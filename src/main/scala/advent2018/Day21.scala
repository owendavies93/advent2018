package advent2018

import scalaadventutils.Problem

object Day21 {

    import advent2018.VM._

    def main(args: Array[String]): Unit = {
        val lines = Problem.parseInputToList("day21")
        val comms = Day19.parseInput(lines)
        println(part1(4, comms))
        println(part2(4, comms))
    }

    def part1(ip: Int, comms: Array[(Comm, List[Int])]): Int = {
        def run_(regs: Regs): Int = {
            val ptr = regs(ip)
            /*
                The eqrr instruction is the only time register 0
                is inspected - so we can just check the thing it's
                compared to (register 1) and return that.
            */
            if (ptr == 28 || ptr < 0 || ptr >= comms.length) regs(1)
            else {
                val (c, vals) = comms(ptr)
                val newR = c.run(vals(0), vals(1), vals(2), regs)
                run_(newR.updated(ip, newR(ip) + 1))
            }
        }

        run_(Map[Int, Int]().withDefaultValue(0))
    }

    def part2(ip: Int, comms: Array[(Comm, List[Int])]): Int = {
        def run_(regs: Regs, seen: Set[Int], prev: Int): Int = {
            val ptr = regs(ip)

            /*
                Same principle as part 1 but now we need the longest
                number of instructions, so just watch for when the
                value of register 1 repeats and return the previous
                value of register 1.
            */
            if (ptr < 0 || ptr >= comms.length) regs(1)
            else if (ptr == 28) {
                if (seen.contains(regs(1))) prev
                else {
                    val (c, vals) = comms(ptr)
                    val newR = c.run(vals(0), vals(1), vals(2), regs)
                    run_(newR.updated(ip, newR(ip) + 1), seen + regs(1), regs(1))
                }
            } else {
                val (c, vals) = comms(ptr)
                val newR = c.run(vals(0), vals(1), vals(2), regs)
                run_(newR.updated(ip, newR(ip) + 1), seen, prev)
            }
        }

        run_(Map[Int, Int]().withDefaultValue(0), Set[Int](), 0)
    }
}


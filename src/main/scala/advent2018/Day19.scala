package advent2018

import scalaadventutils.Problem
import scalaadventutils.Factorise

object Day19 {

    import advent2018.VM._

    def main(args: Array[String]): Unit = {
        val lines = Problem.parseInputToList("day19")
        println(run(3, Map[Int, Int]().withDefaultValue(0), lines))

        /*
            For part 2, after some reverse engineering we find that
            the main portion of the instructions are factorising a
            big number. Run the code until a suitably big number is
            stored in a register, then factor it
        */
        val target = 10551381
        println(Factorise.factors(target).sum)
    }

    def run(ip: Int, init: Map[Int, Int], lines: List[String]): Int = {
        val comms = parseInput(lines)

        def run_(regs: Regs): Int = {
            val ptr = regs(ip)
            if (ptr < 0 || ptr >= comms.length) regs(0)
            else {
                val (c, vals) = comms(ptr)
                val newR = c.run(vals(0), vals(1), vals(2), regs)
                run_(newR.updated(ip, newR(ip) + 1))
            }
        }

        run_(init)
    }

    def parseInput(lines: List[String]): Array[(Comm, List[Int])] =
        lines.map(l => {
            val s = l.split(" ")
            (commMap(s(0)), (1 to 3).map(s(_).toInt).toList)
        }).toArray

    private def commMap(comm: String): Comm = comm match {
        case "addr" => Addr()
        case "addi" => Addi()
        case "mulr" => Mulr()
        case "muli" => Muli()
        case "banr" => Banr()
        case "bani" => Bani()
        case "borr" => Borr()
        case "bori" => Bori()
        case "setr" => Setr()
        case "seti" => Seti()
        case "gtir" => Gtir()
        case "gtri" => Gtri()
        case "gtrr" => Gtrr()
        case "eqir" => Eqir()
        case "eqri" => Eqri()
        case "eqrr" => Eqrr()
    }
}

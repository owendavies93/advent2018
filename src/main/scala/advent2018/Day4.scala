package advent2018

import scalaadventutils.Problem

object Day4 {

    val idParse = """#\d+""".r
    val time    = """:\d+""".r

    def main(args: Array[String]) {
        val lines = Problem.parseInputToList("day4-sorted")
        println(part1(lines))
        println(part2(lines))
    }

    def part1(lines: List[String]) = {
        val guards = parseInput(lines).toList
        val mostSleep = guards.maxBy(_.totalSleep)
        mostSleep.mostSleptMinute._1 * mostSleep.id
    }

    def part2(lines: List[String]) = {
        val guards = parseInput(lines).toList
        val mostSleptMin = guards.maxBy(_.mostSleptMinute._2.size)
        mostSleptMin.mostSleptMinute._1 * mostSleptMin.id
    }

    def parseInput(lines: List[String]) = {
        def parse
            ( durs: List[(Int, Duration)]
            , cur: Int
            , rest: List[String])
            : List[(Int, Duration)] =
            rest match {
                case Nil => durs
                case s :: e :: r
                    if idParse.findFirstMatchIn(s) == None &&
                       time.findFirstMatchIn(s) != None &&
                       time.findFirstMatchIn(e) != None => {
                        val start = time.findFirstMatchIn(s).get
                                        .toString.split(":")(1).toInt
                        val end   = time.findFirstMatchIn(e).get
                                        .toString.split(":")(1).toInt
                        parse(durs :+ (cur, Duration(start, end)), cur, r)
                    }
                case s :: r
                    if idParse.findFirstMatchIn(s) != None => {
                        val id = idParse.findFirstMatchIn(s).get
                                         .toString.split("#")(1).toInt
                        parse(durs, id, r)
                    }
            }

        val durations = parse(List[(Int, Duration)](), 0, lines)

        durations.groupBy(_._1.toInt).mapValues(_.map(_._2))
                 .map(d => Guard(d._1, d._2))
    }

    case class Guard(id: Int, sleeps: List[Duration]) {
        def totalSleep = sleeps.map(s => s.end - s.start).sum

        def mostSleptMinute = sleeps.flatMap(s =>
            (s.start until s.end)
        ).groupBy(identity).maxBy(_._2.size)
    }

    case class Duration(start: Int, end: Int)

}

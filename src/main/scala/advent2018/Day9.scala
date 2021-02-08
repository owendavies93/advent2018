package advent2018

import scalaadventutils.CircularList

import annotation.tailrec

object Day9 {

    type Scores = Map[Int, Int]

    def main(args: Array[String]) {
        println(play(491, 71058))
        println(play(491, 7105800))
    }

    def play(players: Int, end: Int) = {
        @tailrec
        def place
            ( ms: CircularList[Int]
            , marble: Int
            , player: Int
            , s: Scores): Scores = {

            val nextP = (player % players) + 1

            if (marble > end) s
            else if (marble % 23 == 0) {
                val (e, updatedMs) = ms.rotate(-8).pop
                val updatedScores  = s.updated(player, s(player) + e + marble)
                place(updatedMs.rotate(1), marble + 1, nextP, updatedScores)
            } else {
                val updatedMs = ms.rotate(1).push(marble)._2
                place(updatedMs, marble + 1, nextP, s)
            }
        }

        place(
            CircularList[Int](end)(0), 1, 1, Map.empty.withDefaultValue(0)
        ).values.max
    }
}

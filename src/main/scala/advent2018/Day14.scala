package advent2018

import scalaadventutils.Profiler.timeMicS

object Day14 {

    def main(args: Array[String]) {
        println(run(236021))
        println(recipeCount("236021"))
    }

    def run(times: Int) = {
        val result = runner.dropWhile(_._3.size < times + 10).take(1)
        result.toList.head._3.drop(times).take(10).mkString
    }

    def recipeCount(target: String) = {
        val targetList = target.toList.map(_.toInt - 48)
        val result = runner.dropWhile(n => {
            if (n._3.size % 1000 == 0) {
                timeMicS { !n._3.containsSlice(targetList) }
            } else {
                !n._3.containsSlice(targetList)
            }
        }).take(1)
        result.toList.head._3.mkString.indexOf(target)
    }

    private def runner = Iterator.iterate((0, 1, Array(3,7)))(next => {
        val (first, second, recipes) = next
        if (recipes.size % 1000 == 0) {
            println(recipes.size)
            timeMicS { step(first, second, recipes) }
        } else {
            step(first, second, recipes)
        }
    })

    def step(first: Int, second: Int, recipes: Array[Int]) = {
        val sum   = first + second
        val extra = (recipes(first) + recipes(second))
                        .toString
                        .toCharArray
                        .map(_.toInt - 48)
        val nextR = recipes ++ extra

        ((first + recipes(first) + 1) % nextR.size,
         (second + recipes(second) + 1) % nextR.size,
         nextR)
    }
}

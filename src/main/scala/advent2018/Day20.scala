package advent2018

import scalaadventutils.Problem

import annotation.tailrec

object Day20 {

    type Tree = Map[Branch, (Int, Int)]

    val empties = """\([^(]+\|\)""".r
    val branch  = """_(\d+)$""".r

    def main(args: Array[String]): Unit = {
        val lines = Problem.parseInputToString("day20")
        println(findFurthestRoom(lines))
    }

    def findFurthestRoom(input: String, target: Int = 1000): Int = {
        val charInput = empties.replaceAllIn(input, "")
                               .drop(1)
                               .toCharArray.toList

        @tailrec
        def find_
            ( tree: Tree
            , rem: List[Char]
            , longest: Int
            , overTarget: Int
            , curr: Branch): (Int, Int) = rem match {

            case List('$') => (longest, overTarget)
            case ('N' | 'E' | 'S' | 'W') :: tail =>
                val newV = tree(curr)._1 + 1
                val newL = if (longest < newV) newV else longest
                val newO = if (newV >= target) overTarget + 1 else overTarget
                find_(
                    tree.updated(curr, (newV, tree(curr)._2)),
                    tail, newL, newO, curr
                )
            case '(' :: tail =>
                val child  = curr.getFirstChild
                val length = tree(curr)._1
                find_(
                    tree.updated(child, (length, length)),
                    tail, longest, overTarget, child
                )
            case ')' :: tail =>
                find_(tree, tail, longest, overTarget, curr.getParent)
            case '|' :: tail =>
                val newB = curr.getNextSibling
                val newV = tree(curr)._2
                find_(
                    tree.updated(newB, (newV, newV)),
                    tail, longest, overTarget, newB
                )
            case _ => throw new RuntimeException
        }

        val (longest, over) =
            find_(Map[Branch, (Int, Int)](Branch("1") -> (0, 0)),
                  charInput, 0, 0, Branch("1"))

        println(over)
        longest
    }

    case class Branch(id: String) {
        def getFirstChild = Branch(id + "_1")

        def getParent = Branch(branch.replaceAllIn(id, ""))

        def getNextSibling: Branch = {
            val current = branch.findFirstIn(id)
            if (current.isDefined) {
                val idNum = current.get.replace("_", "").toInt
                val newId = "_" + (idNum + 1).toString
                Branch(branch.replaceAllIn(id, newId))
            } else {
                this
            }
        }
    }
}

package advent2018

import scalaadventutils.Problem

object Day24 {

    def main(args: Array[String]): Unit = {
        val infection = Problem.parseInputToList("day24-infection")
        val immune    = Problem.parseInputToList("day24-immune")

        val in = parseInput(infection, Infection, 0)
        var im = parseInput(immune, Immune, 0)
        println(fight(in, im))

        /*
            Found the correct boost through binary search
            to avoid having to code the drawn cases
        */
        im = parseInput(immune, Immune, 36)
        println(fight(in, im))
    }

    def fight(infection: List[Group], immune: List[Group]): Int = {

        def round(in: Array[Group], im: Array[Group]): Int = {
            if (in.isEmpty) im.map(_.count).sum
            else if (im.isEmpty) in.map(_.count).sum
            else {
                val all = (in ++ im).sortBy(g => (-g.power, -g.init))

                /* target selection */
                val targets = collection.mutable.Map[Int, Int]()
                all.foreach(g => {
                    val seen = targets.values.toSet
                    val other =
                        if (g.team == Immune)
                            all.filter(a => a.count > 0 && a.team == Infection)
                               .filterNot(a => seen.contains(a.hashCode()))
                        else
                            all.filter(a => a.count > 0 && a.team == Immune)
                               .filterNot(a => seen.contains(a.hashCode()))

                    if (other.nonEmpty) {
                        val target = selectTarget(
                            g, other.sortBy(o => (o.power, -o.init)))
                        targets += (g.hashCode() -> target.hashCode())
                    }
                })

                /* attack */
                val initOrder = all.sortBy(g => -g.init)
                initOrder.foreach(g => {
                    if (targets.contains(g.hashCode())) {
                        val key = targets(g.hashCode())

                        val index = initOrder.zipWithIndex.find {
                            case (g_, i) => g_.hashCode() == key
                        }.get._2

                        // Need to use the modified version here because damage
                        // might have changed
                        val updatedTarget = initOrder(index)
                        initOrder(index) = updatedTarget.takeDamage(
                            g.calcDamage(updatedTarget)
                        )
                    }
                })

                val newIm =
                    initOrder.filter(g => g.count > 0 && g.team == Immune)
                val newIn =
                    initOrder.filter(g => g.count > 0 && g.team == Infection)
                round(newIn, newIm)
            }
        }

        round(infection.toArray, immune.toArray)
    }

    // This only works if the caller sorts the targets correctly
    private def selectTarget(g: Group, targets: Array[Group]) =
        targets.sortBy(x => (-x.power, -x.init)).maxBy(x => g.calcDamage(x))

    val parser = """(\d+) units each with (\d+) hit points (.*) ?with an attack that does (\d+) (\w+) damage at initiative (\d+)""".r

    def parseInput(lines: List[String], t: Team, boost: Int): List[Group] =
        lines.map(l => l match {
            case parser(count, hp, weak, damage, aType, init) =>
                val (wk, imm) = getWandI(weak)
                val u = GroupUnit(hp.toInt,
                                  damage.toInt,
                                  strToType(aType),
                                  init.toInt, wk, imm)

                Group(List.fill(count.toInt)(u), count.toInt, t, boost)
        })

    def getWandI(wandi: String): (Set[AttackType], Set[AttackType]) = {
        if (wandi == "") (Set[AttackType](), Set[AttackType]())
        else {
            val stripped = wandi.replace("(", "").replace(")", "")
            if (stripped.startsWith("weak")) {
                val s = stripped.split("; ")
                if (s.size == 1) {
                    val weaknesses: Set[AttackType] =
                        stripped.replace("weak to ", "").split(", ")
                                .map(_.trim).map(strToType).toSet
                    (weaknesses, Set[AttackType]())
                } else {
                    val immunities: Set[AttackType] =
                        s(0).replace("weak to ", "").split(", ")
                            .map(_.trim).map(strToType).toSet

                    val weaknesses: Set[AttackType] =
                        s(1).replace("immune to ", "").split(", ")
                            .map(_.trim).map(strToType).toSet
                    (weaknesses, immunities)
                }
            } else {
                val s = stripped.split("; ")
                if (s.size == 1) {
                    val immunities: Set[AttackType] =
                        stripped.replace("immune to ", "").split(", ")
                                .map(_.trim).map(strToType).toSet
                    (Set[AttackType](), immunities)
                } else {
                    val immunities: Set[AttackType] =
                        s(0).replace("immune to ", "").split(", ")
                            .map(_.trim).map(strToType).toSet

                    val weaknesses: Set[AttackType] =
                        s(1).replace("weak to ", "").split(", ")
                            .map(_.trim).map(strToType).toSet
                    (weaknesses, immunities)
                }
            }
        }
    }

    sealed trait AttackType

    case object Fire        extends AttackType
    case object Cold        extends AttackType
    case object Slashing    extends AttackType
    case object Radiation   extends AttackType
    case object Bludgeoning extends AttackType

    def strToType(str: String): AttackType = str match {
        case "fire"        => Fire
        case "cold"        => Cold
        case "slashing"    => Slashing
        case "radiation"   => Radiation
        case "bludgeoning" => Bludgeoning
        case _             => throw new RuntimeException(str)
    }

    case class GroupUnit
        ( hp: Int
        , damage: Int
        , attackType: AttackType
        , initiative: Int
        , weaknesses: Set[AttackType]
        , immunities: Set[AttackType])

    sealed trait Team

    case object Infection extends Team
    case object Immune    extends Team

    case class Group
        ( units: List[GroupUnit]
        , count: Int
        , team: Team
        , boost: Int) {

        def aType  = units(0).attackType
        def damage = units(0).damage
        def hp     = units(0).hp
        def immune = units(0).immunities
        def init   = units(0).initiative
        def power  = count * (damage + boost)
        def weak   = units(0).weaknesses

        def calcDamage(that: Group) = {
            if (that.immune(aType)) 0
            else if (that.weak(aType)) power * 2
            else power
        }

        def takeDamage(d: Int) = {
            val killed = d / hp
            if (killed >= count) copy(count = 0)
            else copy(count = count - killed)
        }

        override def hashCode() =
            damage.## + immune.## + weak.##
    }
}

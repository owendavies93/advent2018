package advent2018

object VM {
    type Regs = Map[Int, Int]

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

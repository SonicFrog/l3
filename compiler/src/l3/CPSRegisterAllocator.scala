package l3

import l3.{ SymbolicCPSTreeModuleLow => S }
import l3.{ RegisterCPSTreeModule => R }

/**
  * A simple register allocator for CPS/L₃.
  *
  * Calling conventions:
  *   I0      contains caller's Ib
  *   I1      contains caller's Lb
  *   I2      contains caller's Ob
  *   I3      contains return address
  *   I4..I31 contain arguments 1 to 28
  *   I4/O0   contains return value (copied by RET instruction)
  *   Ob, Lb  are initially zero
  *
  * Parallel-move algorithm taken from "Tilting at windmills with Coq"
  * by Rideau et al.
  *
  * @author Michel Schinz <Michel.Schinz@epfl.ch>
  */

object CPSRegisterAllocator extends (S.Tree => R.Tree) {
  def apply(tree: S.Tree): R.Tree =
    transform(tree, initialState(tree))

  private val I3 = ASMRegisterFile.in(3)
  private val I4 = ASMRegisterFile.in(4)
  private val O0 = ASMRegisterFile.out(0)

  private def transform(tree: S.Tree, s: State): R.Tree = tree match {
    case S.LetL(name, value, body) =>
      s.withFreshRegFor(name, tree) { (r, s) =>
        R.LetL(r, value, transform(body, s))
      }

    case S.LetP(name, prim, args, body) =>
      s.withFreshRegFor(name, tree) { (r, s) =>
        s.withRegsContaining(args, tree) { (rArgs, s) =>
          R.LetP(r, prim, rArgs, transform(body, s))
        }
      }

    case S.LetC(cnts, body) =>
      s.withContinuations(cnts) { s =>
        val s1 = (s /: cnts) { (s, c) =>
          if (s.retConts(c.name)) {
            assume(c.args.length == 1)
            s.withCntArgs(c.name, Seq(R.Reg(O0)))
          } else {
            s.withFreshRegsFor(c.args, c.body) { (rs, s) =>
              s.withCntArgs(c.name, rs)
            }
          }
        }
        R.LetC(cnts map (transform(_, s1)), transform(body, s1))
      }

    case S.LetF(funs, body) =>
      R.LetF(funs map transform, transform(body, s))

    case S.AppC(cont, args) =>
      s.withRegsContaining(args, tree) { (rArgs, s) =>
        val rOutC = s.cArgs(cont)
        s.withParallelCopy(rOutC, rArgs, tree)(
          R.AppC(s.rOrL(cont), rOutC))
      }

    case S.AppF(fun, retC, args) =>
      s.withRegContaining(fun, tree) { (rFun, s) =>
        s.withRegsContaining(args, tree) { (rArgs, s) =>
          val rOutF = ccOutRegs(args.length)
          s.withParallelCopy(rOutF, rArgs, tree)(
            R.AppF(rFun, s.rOrL(retC), rOutF))
        }
      }

    case S.If(cond, args, thenC, elseC) =>
      R.If(cond, args map s.regs, R.Label(thenC), R.Label(elseC))

    case S.Halt(arg) =>
      R.Halt(s.regs(arg))
  }

  private def transform(cnt: S.CntDef, s: State): R.CntDef = {
    if (s.retConts(cnt.name))
      s.withFreshRegFor(cnt.args.head, cnt.body) { (r, s) =>
        R.CntDef(R.Label(cnt.name),
                 Seq(R.Reg(O0)),
                 R.LetP(r, CPSId, Seq(R.Reg(O0)), transform(cnt.body, s))) }
    else
      R.CntDef(R.Label(cnt.name), s.cArgs(cnt.name), transform(cnt.body, s))
  }

  private def transform(fun: S.FunDef): R.FunDef = {
    val rArgs = ccInRegs(fun.args.length)
    val s = (initialState(fun.body)
               .withAssignedReg(fun.retC, R.Reg(I3))
               .withAssignedRegs(fun.args, rArgs)
               .copy(cArgs = Map(fun.retC -> Seq(R.Reg(I4)))))
    R.FunDef(R.Label(fun.name), R.Reg(I3), rArgs, transform(fun.body, s))
  }

  private case class State(retConts: Set[S.Name],
                           cLiveVars: Map[S.Name, Set[S.Name]] = Map.empty,
                           regs: Map[S.Name, R.Reg] = Map.empty,
                           cArgs: Map[S.Name, Seq[R.Reg]] = Map.empty) {
    def withAssignedReg(name: S.Name, reg: R.Reg) =
      copy(regs = regs + (name -> reg))
    def withAssignedRegs(names: Seq[S.Name], regs: Seq[R.Reg]) = {
      require(names.length == regs.length)
      (this /: (names zip regs)) { case (s, (n, r)) => s.withAssignedReg(n, r) }
    }

    def withCntArgs(name: S.Name, args: Seq[R.Reg]) =
      copy(cArgs = cArgs + (name -> args))

    def withContinuations[T](cnts: Seq[S.CntDef])
                         (body: State => T): T = {
      val emptyCLiveVars = Map(cnts map { c => c.name -> Set[S.Name]() } : _*)
      val cLiveVars1 = fixedPoint(emptyCLiveVars) { cLiveVarsApprox =>
        val s1 = copy(cLiveVars = cLiveVars ++ cLiveVarsApprox)
        Map(cnts map { c => c.name -> s1.liveVariables(c.body) } : _*)
      }
      body(copy(cLiveVars = cLiveVars ++ cLiveVars1))
    }

    def withFreshRegFor[T](name: S.Name, cont: S.Tree)
                       (body: (R.Reg, State) => T): T =
      withFreshRegsFor(Seq(name), cont) { case (Seq(r), s) => body(r, s) }

    def withFreshRegsFor[T](names: Seq[S.Name], cont: S.Tree)
                        (body: (Seq[R.Reg], State) => T): T = {
      val live = liveVariables(cont) flatMap ((regs andThen (_.reg)).lift(_))
      val free =
        ASMRegisterFile.local filterNot live  take names.length  map R.Reg
      assert(free.length == names.length,
             s"not enough local registers (${names.length} requested)")
      body(free, withAssignedRegs(names, free))
    }

    def withRegContaining(name: S.Name, cont: S.Tree)
                         (body: (R.Reg, State) => R.Tree): R.Tree =
      (regs get name map (body(_, this))) getOrElse {
        withFreshRegFor(name, cont) { (r, s) =>
          R.LetP(r, CPSId, Seq(R.Label(name)), body(r, s)) }
      }

    def withRegsContaining(names: Seq[S.Name], cont: S.Tree)
                          (body: (Seq[R.Reg], State) => R.Tree): R.Tree =
      names match {
        case Seq() =>
          body(Seq(), this)
        case Seq(n, ns @ _*) =>
          withRegContaining(n, cont) { (rN, s) =>
            withRegsContaining(ns, cont) { (rNs, s) => body(rN +: rNs, s) } }
      }

    def withParallelCopy(toS: Seq[R.Reg], fromS: Seq[R.Reg], cont: S.Tree)
                        (body: R.Tree): R.Tree = {
      type Move = (R.Reg, R.Reg)

      def splitMove(t: Seq[Move], d: R.Reg)
          : Option[(Seq[Move], R.Reg, Seq[Move])] =
        (t span (_._1 != d)) match {
          case (_, Seq())            => None
          case (pre, (_, r) +: post) => Some(pre, r, post)
        }

      def loop(toMove: Seq[Move], beingMoved: Seq[Move], moved: Seq[Move])
          : Seq[Move] = {
        (toMove, beingMoved, moved) match {
          case (Seq(), Seq(), m) =>
            m.reverse
          case ((s, d) +: tl, b @ Seq(), m) if s == d =>
            loop(tl, b, m)
          case (t +: ts, Seq(), m) =>
            loop(ts, Seq(t), m)
          case (t, (sd @ (s, d)) +: b, m) =>
            splitMove(t, d) match {
              case Some((t1, r, t2)) =>
                loop(t1 ++ t2, (d, r) +: sd +: b, m)
              case None =>
                b match {
                  case Seq() =>
                    loop(t, Seq(), sd +: m)
                  case _ if b.last._1 == d =>
                    val temp = Symbol.fresh("parMoveTmp")
                    withFreshRegFor(temp, cont) { (tmp, _) =>
                      loop(t, b.init :+ ((tmp, b.last._2)), sd +: (d, tmp) +: m)
                    }
                  case _ =>
                    loop(t, b, sd +: m)
                }
            }
        }
      }
      val moves = loop(fromS zip toS, Seq.empty, Seq.empty)
      (moves :\ body) { case ((s, d), b) =>
          R.LetP(d, CPSId, Seq(s), b)
      }
    }

    def rOrL(name: S.Name): R.Name =
      regs.getOrElse(name, R.Label(name))

    def liveVariables(tree: S.Tree): Set[S.Name] = tree match {
      case S.LetL(_, _, body) =>
        liveVariables(body)
      case S.LetP(_, _, args, body) =>
        liveVariables(body) ++ args
      case S.LetC(cnts, body) =>
        val s1 = copy(cLiveVars =
                        cLiveVars ++ (cnts map { c => c.name -> Set[S.Name]()}))
        s1.liveVariables(body) ++ (
          cnts flatMap { c => s1.liveVariables(c.body) })
      case S.LetF(_, body) =>
        liveVariables(body)
      case S.AppC(cont, args) =>
        cLiveVars.getOrElse(cont, Set.empty) ++ args
      case S.AppF(_, retC, args) =>
        cLiveVars.getOrElse(retC, Set.empty) ++ args
      case S.If(_, args, thenC, elseC) =>
        cLiveVars(thenC) ++ cLiveVars(elseC) ++ args
      case S.Halt(arg) =>
          Set(arg)
    }
  }

  private def initialState(tree: S.Tree): State = {
    def retContsT(tree: S.Tree): Set[S.Name] = tree match {
      case S.LetL(_, _, body) => retContsT(body)
      case S.LetP(_, _, _, body) => retContsT(body)
      case S.LetC(cnts, body) => retContsT(body) ++ (cnts flatMap retContsC)
      case S.LetF(_, body) => retContsT(body)
      case S.AppF(_, retC, _) => Set(retC)
      case S.AppC(_, _) | S.If(_, _, _, _) | S.Halt(_) => Set.empty
    }

    def retContsC(cnt: S.CntDef): Set[S.Name] =
      retContsT(cnt.body)

    State(retConts = retContsT(tree))
  }

  private def ccInRegs(n: Int): Seq[R.Reg] = {
    assert(n <= ASMRegisterFile.in.length - 4,
           s"not enough input registers ($n requested)")
    ASMRegisterFile.in drop 4 take n map R.Reg
  }

  private def ccOutRegs(n: Int): Seq[R.Reg] = {
    assert(n <= ASMRegisterFile.out.length - 4,
           s"not enough output registers ($n requested)")
    ASMRegisterFile.out drop 4 take n map R.Reg
  }
}

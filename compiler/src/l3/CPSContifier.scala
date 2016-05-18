package l3

import SymbolicCPSTreeModule._

object CPSContifier extends (Tree => Tree) {
  def apply(t: Tree): Tree =
    fixedPoint(t)(transformT)

  private def transformT(t: Tree): Tree = t match {
    case LetL(name, value, body) =>
      LetL(name, value, transformT(body))
    case LetP(name, prim, args, body) =>
      LetP(name, prim, args, transformT(body))
    case LetC(cnts, body) =>
      LetC(cnts map transformC, transformT(body))
    case LetF(funs, body) =>
      val lf1 = LetF(funs map transformF, transformT(body))
      (lf1 /: contifiables(lf1)) { case (lf, (ns, rc)) =>
          contify(lf, ns.toSet, rc) }
    case AppC(_, _) | AppF(_, _, _) | If(_, _, _, _) | Halt(_) =>
      t
  }

  private def transformC(c: CntDef): CntDef =
    CntDef(c.name, c.args, transformT(c.body))

  private def transformF(f: FunDef): FunDef =
    FunDef(f.name, f.retC, f.args, transformT(f.body))

  private def contifiables(lf: LetF): Seq[(Seq[Name], Name)] = {
    def tailCalls(funDef: FunDef): Set[Name] = {
      def tailCallsT(t: Tree): Set[Name] = t match {
        case LetL(_, _, body) =>
          tailCallsT(body)
        case LetP(_, _, _, body) =>
          tailCallsT(body)
        case LetC(cs, body) =>
          (tailCallsT(body) /: cs) { case (ts, c) => ts ++ tailCallsT(c.body) }
        case LetF(_, body) =>
          tailCallsT(body)
        case AppF(f, c, _) if c == funDef.retC =>
          Set(f)
        case AppC(_, _) | AppF(_, _, _) | If(_, _, _, _) | Halt(_) =>
          Set.empty
      }
      tailCallsT(funDef.body)
    }

    def commonRetCnt(ns: Set[Name]): Option[Name] = {
      def commonRetCntT(t: Tree)
                       (implicit ignore: Option[Name]): SingleElementSet[Name] =
        t match {
          case LetL(_, _, body) =>
            commonRetCntT(body)
          case LetP(_, _, args, body) =>
            if (args exists ns) Top() else commonRetCntT(body)
          case LetC(cnts, body) =>
            (commonRetCntT(body) /: cnts) { case (rc, c) =>
              rc join commonRetCntT(c.body) }
          case LetF(funs, body) =>
            (commonRetCntT(body) /: funs) { case (rc, f) =>
              val newIgnore = if (ns(f.name)) Some(f.retC) else None
              rc join commonRetCntT(f.body)(newIgnore) }
          case AppC(_, args) =>
            if (args exists ns) Top() else Bot()
          case AppF(fun, retC, args) =>
            if (args exists ns)
              Top()
            else if (ns(fun) && ignore != Some(retC))
              Singleton(retC)
            else
              Bot()
          case If(_, args, _, _) =>
            if (args exists ns) Top() else Bot()
          case Halt(_) =>
            Bot()
        }

      commonRetCntT(lf)(None) match {
        case Singleton(c)  => Some(c)
        case Top() | Bot() => None
      }
    }

    val tailCallsGraph: Graph[Name] = {
      val lfNames = (lf.funs map (_.name)).toSet
      Graph((lf.funs map { f => (f.name, (tailCalls(f) & lfNames).toSeq) })
              .toMap)
    }

    (tailCallsGraph.stronglyConnectedComponents map { scc =>
       commonRetCnt(scc.toSet) map ((scc, _))
     }).flatten
  }

  private def contify(lf: LetF, ns: Set[Name], rc: Name): LetF = {
    def pushDown(wrapWithCnts: Tree => LetC, t: Tree): Tree = {
      def pushIn(t: Tree): (Boolean, Tree) = t match {
        case LetL(name, value, body) =>
          val (pushed, body1) = pushIn(body)
          (pushed, LetL(name, value, body1))
        case LetP(name, prim, args, body) =>
          val (pushed, body1) = pushIn(body)
          (pushed, LetP(name, prim, args, body1))
        case LetC(cnts, body) =>
          val (pushedC, cnts1) = (
            cnts map { cnt =>
              val (pushed, cBody1) = pushIn(cnt.body)
              (pushed, CntDef(cnt.name, cnt.args, cBody1))
            }).unzip
          val (pushed, body1) = pushIn(body)
          val pushedCount = (pushed +: pushedC) count identity
          if (pushedCount > 1)
            (true, wrapWithCnts(t))
          else
            (pushedCount == 1, LetC(cnts1, body1))
        case LetF(funs, body) =>
          val (pushedF, funs1) = (
            funs map { fun =>
              val (pushed, fBody1) = pushIn(fun.body)
              (pushed, FunDef(fun.name, fun.retC, fun.args, fBody1))
            }).unzip
          val (pushed, body1) = pushIn(body)
          val pushedCount = (pushed +: pushedF) count identity
          assume(pushedCount <= 1)
          (pushedCount == 1, LetF(funs1, body1))
        case AppF(fun, _, _) if ns(fun) =>
          (true, wrapWithCnts(t))
        case AppF(_, _, _) | AppC(_, _) | If(_, _, _, _) | Halt(_) =>
          (false, t)
      }
      pushIn(t)._2
    }

    def transformApps(t: Tree): Tree = t match {
      case LetL(name, value, body) =>
        LetL(name, value, transformApps(body))
      case LetP(name, prim, args, body) =>
        LetP(name, prim, args, transformApps(body))
      case LetC(cnts, body) =>
        LetC(cnts map { cnt =>
               CntDef(cnt.name, cnt.args, transformApps(cnt.body)) },
             transformApps(body))
      case LetF(funs, body) =>
        LetF(funs map { fun =>
               FunDef(fun.name, fun.retC, fun.args, transformApps(fun.body)) },
             transformApps(body))
      case AppF(fun, _, args) if ns(fun) =>
        AppC(fun, args)
      case AppC(_, _) | AppF(_, _, _) | If(_, _, _, _) | Halt(_) =>
        t
    }

    val (toContify, untouched) = lf.funs partition { f => ns(f.name) }
    val contified = toContify map { case FunDef(name, retC, args, body) =>
      CntDef(name, args, body subst Substitution(retC, rc))
    }
    (transformApps(pushDown(LetC(contified, _), LetF(untouched, lf.body)))
       .asInstanceOf[LetF])
  }
}

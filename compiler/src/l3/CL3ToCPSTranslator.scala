package l3

import l3.{ SymbolicCL3TreeModule => S }
import l3.{ SymbolicCPSTreeModule => C }

object CL3ToCPSTranslator extends (S.Tree => C.Tree) {
  def apply(tree: S.Tree): C.Tree =
    nonTail(tree) { _ =>
      val z = Symbol.fresh("c0")
      C.LetL(z, IntLit(0), C.Halt(z))
    }

  private def nonTail(tree: S.Tree)(ctx: Symbol=>C.Tree): C.Tree = {
    implicit val pos = tree.pos

    // @unchecked to avoid bogus compiler warnings
    (tree: @unchecked) match {
      case S.Let((name, value) +: rest, body) =>
        nonTail(value)(v =>
          C.LetP(name, L3Id, Seq(v), nonTail(S.Let(rest, body))(ctx)))

      case S.Let(Seq(), body) =>
        nonTail(body)(ctx)

      case _ => ??? // TODO
    }
  }

  private def nonTail_*(trees: Seq[S.Tree])(ctx: Seq[Symbol]=>C.Tree): C.Tree =
    trees match {
      case Seq() =>
        ctx(Seq())
      case t +: ts =>
        nonTail(t)(tSym => nonTail_*(ts)(tSyms => ctx(tSym +: tSyms)))
    }

  private def tail(tree: S.Tree, c: Symbol): C.Tree = {
    implicit val pos = tree.pos

    // @unchecked to avoid bogus compiler warnings
    (tree: @unchecked) match {
      case S.Let((name, value) +: rest, body) =>
        nonTail(value)(v =>
          C.LetP(name, L3Id, Seq(v), tail(S.Let(rest, body), c)))

      case S.Let(Seq(), body) =>
        tail(body, c)

      case _ => ??? // TODO
    }
  }

  private def cond(tree: S.Tree, trueC: Symbol, falseC: Symbol): C.Tree = {
    implicit val pos = tree.pos

    def litToCont(l: CL3Literal): Symbol =
      if (l != BooleanLit(false)) trueC else falseC

    tree match {
      case S.If(condE, S.Lit(tl), S.Lit(fl)) =>
        cond(condE, litToCont(tl), litToCont(fl))

      // TODO

      case S.Prim(p: L3TestPrimitive, args) =>
        nonTail_*(args)(as => C.If(p, as, trueC, falseC))

      case other =>
        nonTail(other)(o =>
          nonTail(S.Lit(BooleanLit(false)))(n =>
            C.If(L3Ne, Seq(o, n), trueC, falseC)))
    }
  }

  private def tempLetC(cName: String, args: Seq[C.Name], cBody: C.Tree)
                      (body: C.Name=>C.Tree): C.Tree = {
    val cSym = Symbol.fresh(cName)
    C.LetC(Seq(C.CntDef(cSym, args, cBody)), body(cSym))
  }
}

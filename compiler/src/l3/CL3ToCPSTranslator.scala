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

      case S.Ident(x) => {
        ctx(x)
      }

      case S.Lit(v) => {
        val name = Symbol.fresh("l")
        val body = ctx(name)
        C.LetL(name, v, body)
      }

      case S.LetRec(funs, body) => {
        val newFuns = funs.map(
          { case S.FunDef(f, args, fbody) =>
            val cname = Symbol.fresh("c")
            C.FunDef(f, cname, args, tail(fbody, cname))
          }
        )
        C.LetF(newFuns, nonTail(body)(ctx))
      }

      case S.App(fun, args) => {
        val c = Symbol.fresh("c")

        nonTail_*(fun +: args)(l => {
          C.LetC(Seq(C.CntDef(c, Seq(c), ctx(c))),
            (C.AppF(l.head, c, l.tail)))
        })
      }

      case S.If(e1, e2, e3) =>
        e1 match {
          case S.Prim(p, args) => {
            p match {
              case p : C.TestPrimitive => {
                val r = Symbol.fresh("r")
                tempLetC("c", Seq(r), ctx(r))(x =>
                  tempLetC("ct", Seq(), tail(e2, x))(y =>
                    tempLetC("cf", Seq(), tail(e3, x))(z =>
                      nonTail_*(args)(largs => {
                        C.If(p, largs, y, z)
                          })
                    )
                  )
                )
              }
              case _ => throw new Exception(s"Invalid primitive $p")
            }
          }

          case _ => {
            val r = Symbol.fresh("r")

            tempLetC("c", Seq(r), ctx(r))(x =>
              tempLetC("ct", Seq(), tail(e2, x))(y =>
                tempLetC("cf", Seq(), tail(e3, x))(z =>
                  cond(e1, y, z)
                )
              )
            )
          }
        }

      case S.Prim(p, args) => {
        p match {
          case prim : C.TestPrimitive => {
            nonTail(S.If(tree, S.Lit(BooleanLit(true)), S.Lit(BooleanLit(false))))(ctx)
          }

          case prim : C.ValuePrimitive => {
            val v = Symbol.fresh("p")
            nonTail_*(args)(l => {
              C.LetP(v, prim, l, ctx(v))
            })
          }

          case _ => throw new Exception(s"Invalid primitive $p")
        }
      }


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

      case S.LetRec(funs, body) => {
        val newFuns =  funs.map({
          case S.FunDef(name, args, fbody) => C.FunDef(name, c, args, tail(fbody, c))
        })
        C.LetF(newFuns, tail(body, c))
      }

      case S.App(fun, args) => {
        nonTail_*(fun +: args)(l => {
          C.AppF(l.head, c, l.tail)
        })
      }

      case S.If(e1, e2, e3) => e1 match {
        case S.Prim(p, args) => {
          p match {
            case p : C.TestPrimitive => {
              tempLetC("ct", Seq(), tail(e2, c))(ct =>
                tempLetC("cf", Seq(), tail(e3, c))(cf =>
                  nonTail_*(args)(l =>
                    C.If(p, l, ct, cf)
                  )
                )
              )
            }
          }
        }
        case _ => {
          tempLetC("ct", Seq(), tail(e2, c))(ct =>
            tempLetC("cf", Seq(), tail(e3, c))(cf =>
              cond(e1, ct, cf)
            )
          )
        }
      }

      case S.Prim(p, args) => {
        p match {
          case p : C.TestPrimitive => {
            tail(S.If(tree, S.Lit(BooleanLit(true)), S.Lit(BooleanLit(false))), c)
          }

          case p : C.ValuePrimitive => {
            val pname = Symbol.fresh("p")
            nonTail_*(args)(largs => C.LetP(pname, p, largs, C.AppC(c, Seq(pname))))
          }

          case _ => throw new Exception(s"Invalid primitive $p")
        }
      }

      case other => nonTail(tree)(v => C.AppC(c, Seq(v)))
    }
  }

  private def cond(tree: S.Tree, trueC: Symbol, falseC: Symbol): C.Tree = {
    implicit val pos = tree.pos

    def litToCont(l: CL3Literal): Symbol =
      if (l != BooleanLit(false)) trueC else falseC

    tree match {
      case S.If(condE, S.Lit(tl), S.Lit(fl)) =>
        cond(condE, litToCont(tl), litToCont(fl))

      case S.If(condE, thenE, S.Lit(BooleanLit(false))) =>
        tempLetC("ac", Seq(), cond(thenE, trueC, falseC))(c =>
          cond(condE, c, falseC)
        )

      case S.If(condE, S.Lit(BooleanLit(false)), elseE) =>
        tempLetC("ac", Seq(), cond(elseE, trueC, falseC))(c =>
          cond(condE, falseC, c)
        )

      case S.If(condE, S.Lit(BooleanLit(true)), elseE) =>
        tempLetC("ac", Seq(), cond(elseE, trueC, falseC))(c =>
          cond(condE, trueC, c)
        )

      case S.If(condE, thenE, S.Lit(BooleanLit(true))) =>
        tempLetC("ac", Seq(), cond(thenE, trueC, falseC))(c =>
          cond(condE, c, trueC)
        )

      case S.Prim(p: C.TestPrimitive, args) =>
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

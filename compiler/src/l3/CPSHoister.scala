package l3

import SymbolicCPSTreeModuleLow._

object CPSHoister extends (Tree => Tree) {
  def apply(tree: Tree): Tree = {
    hoist(tree) match {
      case LetF(Seq(), body) => body
      case other => other
    }
  }

  private def hoist(tree: Tree): LetF = tree match {
    case LetL(name, value, body) =>
      val LetF(funs, hBody) = hoist(body)
      LetF(funs, LetL(name, value, hBody))

    case LetP(name, prim, args, body) =>
      val LetF(funs, hBody) = hoist(body)
      LetF(funs, LetP(name, prim, args, hBody))

    case LetC(cnts, body) =>
      val LetF(funs, hBody) = hoist(body)
      val (funDefs, hCnts) = cnts.map(hoistC).unzip
      LetF(funDefs.flatten ++ funs, LetC(hCnts, hBody))

    case LetF(funs, body) =>
      val LetF(hFuns, hBody) = hoist(body)
      LetF(funs.flatMap(hoistF) ++ hFuns, hBody)

    case _ =>
      LetF(Seq(), tree)
  }

  private def hoistC(cnt: CntDef): (Seq[FunDef], CntDef) = {
    val LetF(funs, hBody) = hoist(cnt.body)
    (funs, CntDef(cnt.name, cnt.args, hBody))
  }

  private def hoistF(fun: FunDef): List[FunDef] = {
    val LetF(funs, hBody) = hoist(fun.body)
    FunDef(fun.name, fun.retC, fun.args, hBody) +: funs.toList
  }
}

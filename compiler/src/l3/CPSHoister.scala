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
      val (funDefs, hCnts) =
        cnts.foldLeft((Seq[FunDef](), Seq[CntDef]())){
          case ((fDef, cDef), CntDef(name, args, cBody)) =>
            val LetF(fCnts, hBCnts) = hoist(cBody)
            (fDef ++ fCnts, cDef :+ CntDef(name, args, hBCnts))
        }
      LetF(funDefs ++ funs, LetC(hCnts, hBody))

    case LetF(funs, body) =>
      val LetF(hFuns, hBody) = hoist(body)
      val (fsi, fi)  = funs.foldLeft((Seq[FunDef](), Seq[FunDef]())){
          case ((fsn, fn), FunDef(name, retC, args, fBody)) =>
            val LetF(fCnts, hBFDefs) = hoist(fBody)
            (fsn ++ fCnts, fn :+ FunDef(name, retC, args, hBFDefs))
        }
      LetF(fi ++ fsi ++ hFuns, hBody)

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

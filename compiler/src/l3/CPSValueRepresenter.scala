package l3

import BitTwiddling.bitsToIntMSBF
import l3.{ SymbolicCPSTreeModule => H }
import l3.{ SymbolicCPSTreeModuleLow => L }

/**
  * Value-representation phase for the CPS language. Translates a tree
  * with high-level values (blocks, integers, booleans, unit) and
  * corresponding primitives to one with low-level values (blocks
  * and integers only) and corresponding primitives.
  *
  * @author Michel Schinz <Michel.Schinz@epfl.ch>
  */

object CPSValueRepresenter extends (H.Tree => L.Tree) {
  def apply(tree: H.Tree): L.Tree =
    transform(tree)(Map.empty)

  private def transform(tree: H.Tree)(implicit worker: Map[Symbol, (Symbol, Seq[Symbol])]): L.Tree = tree match {
    case H.LetL(name, CharLit(value), body) =>
      L.LetL(name, (value << 3) | bitsToIntMSBF(1, 1, 0), transform(body))

    case H.LetP(name, L3IntAdd, args, body) =>
      tempLetP(CPSAdd, args) { r =>
        tempLetL(1) { c1 =>
          L.LetP(name, CPSSub, Seq(r, c1), transform(body)) } }

    case H.LetP(name, L3IntSub, args, body) =>
      tempLetP(CPSSub, args) { r =>
        tempLetL(1) { c1 =>
          L.LetP(name, CPSOr, Seq(r, c1), transform(body)) } }

    case H.LetP(name, L3IntDiv, Seq(n1, n2), body) =>
      tempLetL(1) { c1 =>
        tempLetP(CPSSub, Seq(n1, c1)) { n1Rdy =>
          tempLetP(CPSArithShiftR, Seq(n2, c1)) { n2Rdy =>
            tempLetP(CPSDiv, Seq(n1Rdy, n2Rdy)) { r =>
              L.LetP(name, CPSOr, Seq(r, c1), transform(body)) } } } }

    case H.LetP(name, L3IntMul, Seq(n1, n2), body) =>
      tempLetL(1) { c1 =>
        tempLetP(CPSSub, Seq(n1, c1)) { n1Rdy =>
          tempLetP(CPSArithShiftR, Seq(n2, c1)) { n2Rdy =>
            tempLetP(CPSMul, Seq(n1Rdy, n2Rdy)) { r =>
              L.LetP(name, CPSOr, Seq(r, c1), transform(body)) } } } }

    case H.LetP(name, L3IntMod, Seq(n1, n2), body) =>
      tempLetL(1) { c1 =>
        tempLetP(CPSArithShiftR, Seq(n1, c1)) { n1Rdy =>
          tempLetP(CPSArithShiftR, Seq(n2, c1)) { n2Rdy =>
            tempLetP(CPSMod, Seq(n1Rdy, n2Rdy)) { rNShift =>
              tempLetP(CPSArithShiftL, Seq(rNShift, c1)) { r =>
                L.LetP(name, CPSOr, Seq(r, c1), transform(body)) } } } } }

    case H.LetP(name, L3IntArithShiftLeft, Seq(n, s), body) =>
      tempLetL(1) { c1 =>
        tempLetP(CPSSub, Seq(n, c1)) { nRdy =>
          tempLetP(CPSArithShiftR, Seq(s, c1)) { sRdy =>
            tempLetP(CPSArithShiftL, Seq(nRdy, sRdy)) { r =>
              L.LetP(name, CPSOr, Seq(r, c1), transform(body)) } } } }

    case H.LetP(name, L3IntArithShiftRight, Seq(n, s), body) =>
      tempLetL(1) { c1 =>
        tempLetP(CPSArithShiftR, Seq(s, c1)) { sRdy =>
          tempLetP(CPSArithShiftR, Seq(n, sRdy)) { r =>
            L.LetP(name, CPSOr, Seq(r, c1), transform(body)) } } }

    case H.LetP(name, L3IntBitwiseAnd, args, body) =>
      L.LetP(name, CPSAnd, args, transform(body))

    case H.LetP(name, L3IntBitwiseOr, args, body) =>
      L.LetP(name, CPSOr, args, transform(body))

    case H.LetP(name, L3IntBitwiseXOr, args, body) =>
      tempLetL(1) { c1 =>
        tempLetP(CPSXOr, args) { r =>
          L.LetP(name, CPSOr, Seq(r, c1), transform(body)) } }

    case H.LetP(name, L3ByteRead, Nil, body) =>
      tempLetL(1) { c1 =>
        tempLetP(CPSByteRead, Nil) { t1 =>
          tempLetP(CPSArithShiftL, Seq(t1, c1)) { r =>
            L.LetP(name, CPSOr, Seq(r, c1), transform(body)) } } }

    case H.LetP(name, L3ByteWrite, Seq(n), body) =>
      tempLetL(1) { c1 =>
        tempLetP(CPSSub, Seq(n, c1)) { nPreRdy =>
          tempLetP(CPSArithShiftR, Seq(nPreRdy, c1)) { r =>
            L.LetP(name, CPSByteWrite, Seq(r), transform(body)) } } }

    case H.LetP(name, L3IntToChar, Seq(n), body) =>
      tempLetL(2) { c2 =>
        tempLetP(CPSArithShiftL, Seq(n, c2)) { r =>
          L.LetP(name, CPSOr, Seq(r, c2), transform(body)) } }

    case H.LetP(name, L3CharToInt, Seq(n), body) =>
      tempLetL(2) { c2 =>
        L.LetP(name, CPSArithShiftR, Seq(n, c2), transform(body)) }

    case H.LetP(name, L3Id, args, body) =>
      L.LetP(name, CPSId, args, transform(body))

    case H.LetP(name, L3BlockAlloc(tag), Seq(n), body) =>
      tempLetL(1) { c1 =>
        tempLetP(CPSArithShiftR, Seq(n, c1)) { r =>
          L.LetP(name, CPSBlockAlloc(tag), Seq(r), transform(body)) } }

    case H.LetP(name, L3BlockTag, arg, body) =>
      tempLetL(1) { c1 =>
        tempLetP(CPSBlockTag, arg) { read =>
          tempLetP(CPSArithShiftL, Seq(read, c1)) { r =>
            L.LetP(name, CPSOr, Seq(r, c1), transform(body)) } } }

    case H.LetP(name, L3BlockLength, arg, body) =>
      tempLetL(1) { c1 =>
        tempLetP(CPSBlockLength, arg) { len =>
          tempLetP(CPSArithShiftL, Seq(len, c1)) { r =>
            L.LetP(name, CPSOr, Seq(r, c1), transform(body)) } } }

    case H.LetP(name, L3BlockGet, Seq(b, n), body) =>
      tempLetL(1) { c1 =>
        tempLetP(CPSArithShiftR, Seq(n, c1)) { nRdy =>
          L.LetP(name, CPSBlockGet, Seq(b, nRdy), transform(body)) } }

    case H.LetP(name, L3BlockSet, Seq(b, n, v), body) =>
      tempLetL(1) { c1 =>
        tempLetP(CPSArithShiftR, Seq(n, c1)) { nRdy =>
          L.LetP(name, CPSBlockSet, Seq(b, nRdy, v), transform(body)) } }

    case H.If(L3IntP, Seq(a), thenC, elseC) =>
      ifEqLSB(a, Seq(1), thenC, elseC)

    case H.LetC(cnt, body) =>
      L.LetC(cnt.map(x => new L.CntDef(x.name, x.args, transform(x.body))), transform(body))

    case H.AppC(cnt, args) => L.AppC(cnt, args)

    case H.LetL(name, IntLit(v), body) =>
      L.LetL(name, (v << 1) | 1, transform(body))

    case H.LetF(funs, body) =>
      L.LetF (
        funs map (x => L.FunDef(x.name, x.retC, x.args, transform(x.body))),
        transform(body)
      )

    case H.AppF(name, c, args) => L.AppF(name, c, args)

    case H.Halt(name) =>
      tempLetL(1) { c1 =>
        tempLetP(CPSArithShiftR, Seq(name, c1)) { r =>
          L.Halt(r) } }

    case H.If(L3IntLt, args, ct, cf) => L.If(CPSLt, args, ct, cf)

    case H.If(L3IntLe, args, ct, cf) => L.If(CPSLe, args, ct, cf)

    case H.If(L3IntGe, args, ct, cf) => L.If(CPSGe, args, ct, cf)

    case H.If(L3IntGt, args, ct, cf) => L.If(CPSGt, args, ct, cf)

    case H.If(L3Eq, args, ct, cf) => L.If(CPSEq, args, ct, cf)

    case H.If(L3Ne, args, ct, cf) => L.If(CPSNe, args, ct, cf)

    case H.If(L3CharP, Seq(a), ct, cf) => ifEqLSB(a, Seq(1, 1, 0), ct, cf)
    case H.If(L3BoolP, Seq(a), ct, cf) => ifEqLSB(a, Seq(1, 0, 1, 0), ct, cf)
    case H.If(L3UnitP, Seq(a), ct, cf) => ifEqLSB(a, Seq(0, 0, 1, 0), ct, cf)
    case H.If(L3BlockP, Seq(a), ct, cf) => ifEqLSB(a, Seq(0, 0), ct, cf)

    case H.LetL(name, BooleanLit(false), body) =>
      L.LetL(name, bitsToIntMSBF(1, 0, 1, 0), transform(body))

    case H.LetL(name, BooleanLit(true), body) =>
      L.LetL(name, bitsToIntMSBF(1, 1, 0, 1, 0), transform(body))

    case H.LetL(name, UnitLit, body) =>
      L.LetL(name, bitsToIntMSBF(0, 0, 1, 0), transform(body))

    case H.If(L3IntP, Seq(a), thenC, elseC) =>
      ifEqLSB(a, Seq(1), thenC, elseC)

    case _ => ??? // TODO Handle other cases
  }

  private def bolocksAlloc(fun: H.FunDef, w1: H.Name, freeV: Seq[H.Name],
      letFBody: H.Tree)(implicit
      worker: Map[Symbol, (Symbol, Seq[Symbol])]): L.Tree = {
    val f1 = fun.name

    def letArgs(freeVars: Seq[H.Name], idx: Int = 0): L.Tree = {
      tempLetL(idx){ index =>
        freeVars match {
          case Seq(tLast) =>
            tempLetP(CPSBlockSet, Seq(f1, index, tLast))(ti => transform(letFBody))
          case Seq(tCurr, tail@_*) =>
            tempLetP(CPSBlockSet, Seq(f1, index, tCurr)){ti => letArgs(tail)}
        }
      }
    }

    tempLetL(freeV.size + 1){ sz =>
      L.LetP(f1, CPSBlockAlloc(202), Seq(sz), letArgs(freeV.+:(w1)))}
  }

  private def freeVars(tree : H.Tree) : Seq[H.Name] = {
    def F(tree : H.Tree) : Set[H.Name] = tree match {
      case H.LetL(name, _, e) => F(e) - name
      case H.LetP(name, _, args, e) => (F(e) - name) union args.toSet
      case H.LetC(cnts, e) => F(e) union (cnts.map(contFV).fold(Set())(_ union _))
      case H.LetF(funs, e) => F(e) union (funs.map(funFV).fold(Set())(_ union _))
      case H.AppC(_, args) => args.toSet
      case H.AppF(fun, _, args) => args.toSet
      case H.If(_, args, _, _) => args.toSet
      case H.Halt(arg) => Set(arg)
    }

    def funFV(f : H.FunDef) : Set[H.Name] = f match {
      case H.FunDef(_, _, args, e) => F(e) &~ args.toSet
    }

    def contFV(c : H.CntDef) : Set[H.Name] = c match {
      case H.CntDef(_, args, e) => F(e) &~ args.toSet
    }

    F(tree).toSeq
  }

  // Tree builders

  /**
    * Call body with a fresh name, and wraps its resulting tree in one
    * that binds the fresh name to the given literal value.
    */
  private def tempLetL(v: Int)(body: L.Name => L.Tree): L.Tree = {
    val tempSym = Symbol.fresh("t")
    L.LetL(tempSym, v, body(tempSym))
  }

  /**
    * Call body with a fresh name, and wraps its resulting tree in one
    * that binds the fresh name to the result of applying the given
    * primitive to the given arguments.
    */
  private def tempLetP(p: L.ValuePrimitive, args: Seq[L.Name])
    (body: L.Name => L.Tree): L.Tree = {
    val tempSym = Symbol.fresh("t")
    L.LetP(tempSym, p, args, body(tempSym))
  }

  /**
    * Generate an If tree to check whether the least-significant bits
    * of the value bound to the given name are equal to those passed as
    * argument. The generated If tree will apply continuation tC if it
    * is the case, and eC otherwise. The bits should be ordered with
    * the most-significant one first (e.g. the list (1,1,0) represents
    * the decimal value 6).
    */
  private def ifEqLSB(arg: L.Name, bits: Seq[Int], tC: L.Name, eC: L.Name)
      : L.Tree =
    tempLetL(bitsToIntMSBF(bits map { b => 1 } : _*)) { mask =>
      tempLetP(CPSAnd, Seq(arg, mask)) { masked =>
        tempLetL(bitsToIntMSBF(bits : _*)) { value =>
          L.If(CPSEq, Seq(masked, value), tC, eC) } } }
}

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
    transform(tree)

  private def transform(tree: H.Tree): L.Tree = tree match {
    case H.LetL(name, CharLit(value), body) =>
      L.LetL(name, (value << 3) | bitsToIntMSBF(1, 1, 0), transform(body))

    case H.LetP(name, L3IntAdd, args, body) =>
      tempLetP(CPSAdd, args) { r =>
        tempLetL(1) { c1 =>
          L.LetP(name, CPSSub, Seq(r, c1), transform(body)) } }

    case H.If(L3IntP, Seq(a), thenC, elseC) =>
      ifEqLSB(a, Seq(1), thenC, elseC)

    case H.LetC(cnt, body) =>
      L.LetC(cnt.map(x => new L.CntDef(x.name, x.args, transform(x.body))), transform(body))

    case H.AppC(cnt, args) => L.AppC(cnt, args)

    case H.LetL(name, IntLit(v), body) =>
      L.LetL(name, 2 * v + 1, transform(body))

    case H.LetF(funs, body) =>
      L.LetF (
        funs map (x => L.FunDef(x.name, x.retC, x.args, transform(x.body))),
        transform(body)
      )

    case H.If(L3IntLt, args, ct, cf) => L.If(CPSLt, args, ct, cf)

    case H.If(L3IntLe, args, ct, cf) => L.If(CPSLe, args, ct, cf)

    case H.If(L3IntGe, args, ct, cf) => L.If(CPSGe, args, ct, cf)

    case H.If(L3IntGt, args, ct, cf) => L.If(CPSGt, args, ct, cf)

    case H.If(L3CharP, Seq(a), ct, cf) => ifEqLSB(a, Seq(1, 1, 0), ct, cf)
    case H.If(L3BoolP, Seq(a), ct, cf) => ifEqLSB(a, Seq(1, 0, 1, 0), ct, cf)
    case H.If(L3UnitP, Seq(a), ct, cf) => ifEqLSB(a, Seq(0, 0, 1, 0), ct, cf)
    case H.If(L3BlockP, Seq(a), ct, cf) => ifEqLSB(a, Seq(0, 0), ct, cf)

    case H.LetL(name, BooleanLit(true), body) =>
      L.LetL(name, bitsToIntMSBF(1, 0, 1, 0), transform(body))

    case H.LetL(name, BooleanLit(false), body) =>
      L.LetL(name, bitsToIntMSBF(1, 1, 0, 1, 0), transform(body))

    case H.LetL(name, UnitLit, body) =>
      L.LetL(name, bitsToIntMSBF(0, 0, 1, 0), transform(body))

    case _ => ??? // TODO Handle other cases
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

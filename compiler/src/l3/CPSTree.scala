package l3

/**
 * A module for CPS trees.
 *
 * @author Michel Schinz <Michel.Schinz@epfl.ch>
 */

trait CPSTreeModule {
  type Name
  type ValuePrimitive
  type TestPrimitive
  type Literal

  sealed trait Tree {
    /**
     * Produce a new version of the tree where all names have been
     * substituted according to the given (partial) substitution.
     */
    def subst(s: Substitution[Name]): Tree = {
      def substIn(t: Tree): Tree = t match {
        case LetL(name, value, body) =>
          LetL(s(name), value, substIn(body))
        case LetP(name, prim, args, body) =>
          LetP(s(name), prim, args map s, substIn(body))
        case LetC(cnts, body) =>
          val substCnts = cnts map {
            case CntDef(name, args, body) =>
              CntDef(s(name), args map s, substIn(body))
          }
          LetC(substCnts, substIn(body))
        case LetF(funs, body) =>
          val substFuns = funs map {
            case FunDef(name, retC, args, body) =>
              FunDef(s(name), s(retC), args map s, substIn(body))
          }
          LetF(substFuns, substIn(body))
        case AppC(cont, args) =>
          AppC(s(cont), args map s)
        case AppF(fun, retC, args) =>
          AppF(s(fun), s(retC), args map s)
        case If(cond, args, thenC, elseC) =>
          If(cond, args map s, s(thenC), s(elseC))
        case Halt(arg) =>
          Halt(s(arg))
      }

      substIn(this)
    }
  }

  case class LetL(name: Name, value: Literal, body: Tree) extends Tree
  case class LetP(name: Name, prim: ValuePrimitive, args: Seq[Name], body:Tree)
       extends Tree
  case class LetC(cnts: Seq[CntDef], body: Tree) extends Tree
  case class LetF(funs: Seq[FunDef], body: Tree) extends Tree
  case class AppC(cnt: Name, args: Seq[Name]) extends Tree
  case class AppF(fun: Name, retC: Name, args: Seq[Name]) extends Tree
  case class If(cond: TestPrimitive, args: Seq[Name], thenC: Name, elseC: Name)
       extends Tree
  case class Halt(arg: Name) extends Tree

  case class CntDef(name: Name, args: Seq[Name], body: Tree)
  case class FunDef(name: Name, retC: Name, args: Seq[Name], body: Tree)
}

/**
 * Module for "high-level" CPS trees: the full L3 literals and
 * primitives are available.
 */
object SymbolicCPSTreeModule extends CPSTreeModule {
  type Name = Symbol
  type ValuePrimitive = L3ValuePrimitive
  type TestPrimitive = L3TestPrimitive
  type Literal = CL3Literal
}

/**
 * Module for "low-level" CPS trees: the only literal values are
 * integers, and the primitives work on integers and/or pointers to
 * heap-allocated blocks.
 */
object SymbolicCPSTreeModuleLow extends CPSTreeModule {
  type Name = Symbol
  type ValuePrimitive = CPSValuePrimitive
  type TestPrimitive = CPSTestPrimitive
  type Literal = Int
}

/**
 * Module for register-allocated CPS trees: names either represent ASM
 * registers or ASM labels. (Since register names are often reused,
 * names are no longer globally unique as previously).
 */
object RegisterCPSTreeModule extends CPSTreeModule {
  sealed abstract class Name {
    override def toString: String = this match {
      case Reg(r) => r.toString
      case Label(l) => l.toString
    }
  }
  case class Reg(reg: ASMRegister) extends Name
  case class Label(label: Symbol) extends Name

  type ValuePrimitive = CPSValuePrimitive
  type TestPrimitive = CPSTestPrimitive
  type Literal = Int
}

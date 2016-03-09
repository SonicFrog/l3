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

  sealed trait Tree

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


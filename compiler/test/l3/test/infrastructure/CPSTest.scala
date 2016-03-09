package l3.test.infrastructure

import l3._
import l3.CPSTreeModule

/**
 * CPS testing infrastructure
 *
 * @author Vlad Ureche <vlad.ureche@epfl.ch>
 */
class CPSTest[CPSModule <: CPSTreeModule](val CPS: CPSModule) extends CompilerTest with BasicTest {

  /** Creates a new name based on the old name. Note that this needs to be overridden for register-based CPS. */
  def createName(newname: String, oldname: CPS.Name): CPS.Name = new Symbol(newname).asInstanceOf[CPS.Name]

  /** An phase that renames variables in the order they appear in the source code */
  object CPSVariableRenamePhase extends (CPS.Tree => CPS.Tree) {
    import CPS._

    // scope-based transformation - but since we have name
    // analysis, symbols (or names) are unique objects
    private[this] var scope = Map[Name, Name]()

    // name replacement
    private[this] var id = 1
    private[this] def newName(name: Name): Name = {
      val name1 = createName("v$" + id, name)
      scope += name -> name1
      id += 1; name1
    }
    private[this] def updateName(name: Name): Name = scope(name)

    // entry point
    def apply(tree: Tree): Tree = {
      id = 1 // reset the counter to 1
      updateTree(tree)
    }

    // tree traversal logic
    private[this] def updateTree(tree: Tree): Tree = (tree: @unchecked) match {
      case LetL(name, literal, body) =>
        LetL(newName(name), literal, updateTree(body))
      case LetP(name, prim, args, body) =>
        LetP(newName(name), prim, args.map(updateName), updateTree(body))
      case AppC(cnt, args) =>
        AppC(updateName(cnt), args.map(updateName))
      case AppF(fun, retK, args) =>
        AppF(updateName(fun), updateName(retK), args.map(updateName))
      case If(cond, args, thenK, elseK) =>
        If(cond, args.map(updateName), updateName(thenK), updateName(elseK))
      case Halt(n) =>
        Halt(updateName(n))
      /* We have to create the new names for all the functions and continuations
       *  *before* updating the body of any of them. Otherwise forward references
       * to other functions (which are mandatory for mutual recursion) will
       * crash this translator. */
      case LetC(continuations, body) =>
        val newNames = continuations map (cnt => newName(cnt.name))
        LetC(continuations.zip(newNames).map((updateContinuation _).tupled), updateTree(body))
      case LetF(functions, body) =>
        val newNames = functions map (fun => newName(fun.name))
        LetF(functions.zip(newNames).map((updateFunction _).tupled), updateTree(body))
    }

    // ContDef traversal
    private[this] def updateContinuation(cntDef: CntDef, name: Name): CntDef = cntDef match {
      case CntDef(_, args, body) =>
        CntDef(name, args.map(newName), updateTree(body))
    }

    // FunDef traversal
    private[this] def updateFunction(funDef: FunDef, name: Name): FunDef = funDef match {
      case FunDef(_, retK, args, body) =>
        FunDef(name, newName(retK), args.map(newName), updateTree(body))
    }
  }
}

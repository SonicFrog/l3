package l3.test.infrastructure

import l3._
import l3.{ SymbolicCL3TreeModule => S }
import l3.CL3TreeFormatter._

/**
 * L3 testing infrastructure
 *
 * @author Vlad Ureche <vlad.ureche@epfl.ch>
 */
trait L3Test extends CompilerTest with BasicTest {

  /** Checks the equality between the tree produced by compiling the source
   *  and a given tree. Comparison is done string-wise, ignoring white spaces
   *  and replacing names by v$n, where n is increasing with each name
   *  encountered. Used for checking individual rules.
   *
   *  Example:
   *  ```testL3TreeEquality("(def n 3) #u", "(let ((v$1 3)) #u)")```
   */
  def testL3TreeEquality(source: String, expectedTree: String) = {
    val pipeline = () => (CL3NameAnalyzer andThen L3VariableRenamePhase andThen TreeToString)
    val generatedTree = compileUsingPipeline(() => source, pipeline)
    assertEqual(source, "", generatedTree, expectedTree)
  }

  /** Checks the equality between the result produced by compiling and running
   *  the source code for a given input. Comparison is done string-wise,
   *  ignoring white spaces. Used for checking entire programs.
   *
   *  Example:
   *  ```
   *  testL3ProgramOutput("""
   *    (@byte-write (@char->int (if (@unit? (if #f 1)) 'O' 'K')))
   *    (@byte-write (@char->int (if (@unit? (if #t 1)) 'O' 'K')))
   *    (@byte-write 10)
   *  """)
   *  ```
   */
  def testL3ProgramOutput(source: String, input: String = "", expectedOutput: String = "OK") = {
    val pipeline = () => (CL3NameAnalyzer andThen CL3Interpreter)
    val output = compileUsingPipelineAndRedirect(() => source, pipeline, input)
    assertEqual(source, input, output, expectedOutput)
  }

  /** An phase that renames variables in the order they appear in the source code */
  object L3VariableRenamePhase extends (S.Tree => S.Tree) {
    import l3.SymbolicCL3TreeModule._

    // scope-based transformation
    private[this] var scope = Map[Name, Name]()
    private[this] def withSavedScope[T](f: => T): T = {
      val old_scope = scope
      val result = f
      scope = old_scope
      result
    }

    // name replacement
    private[this] var id = 1
    private[this] def replaceName(name: Name): Name = { val name1 = new Symbol("v$" + id); scope += name -> name1; id += 1; name1 }
    private[this] def updateName(name: Name): Name = scope(name)

    // entry point
    def apply(tree: Tree): Tree = {
      id = 1 // reset the counter to 1
      updateTree(tree)
    }

    // tree traversal logic
    private[this] def updateTree(tree: Tree): Tree = {
      implicit val pos = tree.pos
      tree match {
        case Let(bindings, body) =>
          withSavedScope(Let(updateBindings(bindings), updateTree(body)))
        case LetRec(functions, body) =>
          withSavedScope(LetRec(functions.map(updateFunction), updateTree(body)))
        case If(condE, thenE, elseE) =>
          If(updateTree(condE), updateTree(thenE), updateTree(elseE))
        case App(fun, args) =>
          App(updateTree(fun), updateTrees(args))
        case Prim(prim, args) =>
          Prim(prim, updateTrees(args))
        case Lit(value) =>
          tree
        case Ident(name) =>
          Ident(updateName(name))
        case Halt(name) =>
          Halt(updateTree(name))
      }
    }

    private[this] def updateBindings(bindings: Seq[(Name, Tree)]): Seq[(Name, Tree)] = bindings.map(b => (replaceName(b._1), updateTree(b._2)))
    private[this] def updateTrees(trees: Seq[Tree]): Seq[Tree] = trees.map(updateTree)
    private[this] def updateFunction(funDef: FunDef): FunDef = funDef match {
      case FunDef(name, args, body) =>
        val nname = replaceName(name)
        val (nargs, nbody) = withSavedScope((args.map(replaceName), updateTree(body)))
        FunDef(nname, nargs, nbody)(funDef.pos)
    }
  }
}

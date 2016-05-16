package l3.test.infrastructure

import l3._
import l3.CPSTreeFormatter._

/**
 * Low-level CPS testing infrastructure, after the value representation transformation
 *
 * @author Vlad Ureche <vlad.ureche@epfl.ch>
 */
class CPSLowTest extends CPSTest(SymbolicCPSTreeModuleLow) {

  /** Checks the equality between the tree produced by compiling the source
   *  and a given tree. Comparison is done string-wise, ignoring white spaces
   *  and replacing names by v$n, where n is increasing with each name
   *  encountered. Used for checking individual rules.
   */
  def testCPSLowTreeEquality(source: String, expectedTree: String) = {
    val pipeline =
      () => (CL3NameAnalyzer
             andThen reference.CL3ToCPSTranslator
             andThen reference.CPSValueRepresenter
             andThen reference.CPSHoister
             andThen CPSVariableRenamePhase
             andThen TreeToString)
    val generatedTree = compileUsingPipeline(() => source, pipeline)
    assertEqual(source, "", generatedTree, expectedTree)
  }

  /** Checks the equality between the result produced by compiling and running
   *  the source code for a given input. Comparison is done string-wise,
   *  ignoring white spaces. Used for checking entire programs.
   */
  def testCPSLowProgramOutput(source: String, input: String = "", expectedOutput: String = "OK") = {
    val pipeline =
      () => (CL3NameAnalyzer
             andThen reference.CL3ToCPSTranslator
             andThen reference.CPSValueRepresenter
             andThen reference.CPSHoister
             andThen CPSHoistChecker
             andThen CPSInterpreterLow)
    val output = compileUsingPipelineAndRedirect(() => source, pipeline, input)
    assertEqual(source, input, output, expectedOutput)
  }

  object CPSHoistChecker extends (CPS.Tree => CPS.Tree) {
    import CPS._

    // entry point
    def apply(tree: Tree): Tree = {
      visitTree(tree, mayBeLetF = true)
      tree
    }

    // tree traversal logic
    private[this] def visitTree(tree: Tree, mayBeLetF: Boolean = false): Unit = tree match {
      case LetL(name, literal, body) =>
        visitTree(body)
      case LetP(name, prim, args, body) =>
        visitTree(body)
      case LetC(cnts, body) =>
        cnts.foreach(visitContinuation)
        visitTrees(body)
      case LetF(functions, body) =>
        assert(mayBeLetF, "You haven't performed the LetF hoisting: Only one LetF should appear as the tree root.")
        functions.foreach(visitFunction)
        visitTree(body)
      case _ =>
        ()
    }

    // Multiple tree traversal
    private[this] def visitTrees(trees: Tree*): Unit =
      trees.foreach(visitTree(_))

    // FunDef traversal
    private[this] def visitContinuation(cntDef: CntDef): Unit = cntDef match {
      case CntDef(name, args, body) =>
        visitTree(body)
    }

    // FunDef traversal
    private[this] def visitFunction(funDef: FunDef): Unit = funDef match {
      case FunDef(name, retK, args, body) =>
        visitTree(body)
    }
  }

}

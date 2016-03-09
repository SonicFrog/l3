package l3.test.infrastructure

import l3._
import l3.CPSTreeFormatter._

/**
 * High-level CPS testing infrastructure
 *
 * @author Vlad Ureche <vlad.ureche@epfl.ch>
 */
class CPSHighTest extends CPSTest(SymbolicCPSTreeModule) {

  /** Checks the equality between the tree produced by compiling the source
   *  and a given tree. Comparison is done string-wise, ignoring white spaces
   *  and replacing names by v$n, where n is increasing with each name
   *  encountered. Used for checking individual rules.
   */
  def testCPSHighTreeEquality(source: String, expectedTree: String) = {
    val pipeline =
      () => (CL3NameAnalyzer andThen
             CL3ToCPSTranslator andThen
             CPSVariableRenamePhase andThen
             TreeToString)
    val generatedTree = compileUsingPipeline(() => source, pipeline)
    assertEqual(source, "", generatedTree, expectedTree)
  }

  /** Checks the equality between the result produced by compiling and running
   *  the source code for a given input. Comparison is done string-wise,
   *  ignoring white spaces. Used for checking entire programs.
   */
  def testCPSHighProgramOutput(source: String, input: String = "", expectedOutput: String = "OK") = {
    val pipeline = () => (CL3NameAnalyzer andThen CL3ToCPSTranslator andThen CPSInterpreterHigh)
    val output = compileUsingPipelineAndRedirect(() => source, pipeline, input)
    assertEqual(source, input, output, expectedOutput)
  }
}

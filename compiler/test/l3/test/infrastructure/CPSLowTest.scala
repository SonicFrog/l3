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
             andThen CL3ToCPSTranslator
             andThen CPSValueRepresenter
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
             andThen CL3ToCPSTranslator
             andThen CPSValueRepresenter
             andThen CPSInterpreterLow)
    val output = compileUsingPipelineAndRedirect(() => source, pipeline, input)
    assertEqual(source, input, output, expectedOutput)
  }

}

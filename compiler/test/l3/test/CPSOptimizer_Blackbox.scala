package l3.test

import l3.test.infrastructure.CPSOptTest
import l3.test.ok.AllOKTests

/** Blackbox testing for entire program outputs */
class CPSOptimizer_Blackbox extends CPSOptTest with AllOKTests {

  val compileAndInterpret = (src: String) => testCPSLowProgramOutput(source = src)
  // TODO: Add other specific tests here
}

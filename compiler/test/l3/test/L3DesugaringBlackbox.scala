package l3.test

import l3.test.infrastructure.L3Test
import l3.test.ok.AllOKTests

/** Blackbox testing for entire program outputs */
class L3DesugaringBlackbox extends L3Test with AllOKTests {

  val compileAndInterpret = (src: String) => testL3ProgramOutput(source = src)
  // TODO: Add other specific tests here
}

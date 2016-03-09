package l3.test

import l3.test.infrastructure.CPSHighTest
import l3.test.ok.AllOKTests

/** Blackbox testing for entire program outputs */
class CL3ToCPS_Blackbox extends CPSHighTest with AllOKTests {

  val compileAndInterpret = (src: String) => testCPSHighProgramOutput(source = src)
  // TODO: Add other specific tests here
}

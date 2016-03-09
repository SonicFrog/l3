package l3.test

import l3.test.infrastructure.CPSHighTest
import org.junit.Test

/** Whitebox testing for entire program outputs */
class CL3ToCPS_Whitebox_NonTail extends CPSHighTest {

  @Test def testNonTailLiteral =
    testCPSHighTreeEquality("3", "(let* ((v$1 3) (v$2 0)) (halt v$2))")

  @Test def testNonTailMultiLet =
    testCPSHighTreeEquality("(let ((x 1) (y 2)) y)",
        "(let* ((v$1 1) (v$2 (@id v$1)) (v$3 2) (v$4 (@id v$3)) (v$5 0)) (halt v$5))")

  // TODO: Add more tests here
}

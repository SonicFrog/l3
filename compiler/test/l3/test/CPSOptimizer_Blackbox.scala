package l3.test

import l3.test.infrastructure.CPSOptTest
import l3.test.ok.AllOKTests

import org.junit.Test

/** Blackbox testing for entire program outputs */
class CPSOptimizer_Blackbox extends CPSOptTest with AllOKTests {

  val compileAndInterpret = (src: String) => testCPSLowProgramOutput(source = src)

  @Test def testConstantTest = compileAndInterpret("""
    (let* ((u #u))
       (if (@unit? u) (@byte-write (@char->int 'O')))
       (@byte-write (@char->int 'K')))
  """)

  @Test def testMakePrinterComplex = compileAndInterpret("""
    (def make-printer (fun (x y)
                         (fun ()
                           (@byte-write (@char->int x))
                           (@byte-write (@char->int y)))))
    ((make-printer 'O' 'K'))
    """)
}

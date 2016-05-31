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

  @Test def testOrFolding = compileAndInterpret("""
    (if (and #t #f) (@byte-write (@char->int 'K')))
    (if (or #t #f) (@byte-write (@char->int 'O')))
    (if (and #t #t) (@byte-write (@char->int 'K')))
  """)

  @Test def testBlockSetChanged = compileAndInterpret("""
     (let* ((b (@block-alloc-100 2))
            (c (@block-set! b 0 'O'))
            (d (@block-set! b 1 'O')))
         (@block-set! b 1 'K')
         (@byte-write (@char->int (@block-get b 0)))
         (@byte-write (@char->int (@block-get b 1))))
  """)

  @Test def testClosureOpt = compileAndInterpret("""
    (let* ((f (fun (x y) (fun () (@byte-write (@char->int x)) (@byte-write (@char->int y)))))
           (print (f 'O' 'K')))
       (print))
  """)

  @Test def testBlockCSE = compileAndInterpret("""
    (let* ((g (@block-alloc-50 2))
           (b (@block-alloc-50 2))
           (j (@block-set! g 0 'O'))
           (k (@block-set! g 1 'K'))
           (l (@block-set! b 0 'K'))
           (m (@block-set! b 1 'O')))
       (@byte-write (@char->int (@block-get g 0)))
       (@byte-write (@char->int (@block-get g 1))))
  """)
}

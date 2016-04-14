package l3.test

import l3.test.infrastructure.CPSLowTest
import l3.test.ok.AllOKTests

import org.junit.Test

/** Blackbox testing for entire program outputs */
class CPSValueRepresentation_Blackbox extends CPSLowTest with AllOKTests {

  val compileAndInterpret = (src: String) => testCPSLowProgramOutput(source = src)
  // TODO: Add other specific tests here

  @Test def testMakePrinter = compileAndInterpret("""
     (def make-printer (fun (x)
                         (fun ()
                           (@byte-write (@char->int x))
                           (@byte-write (@char->int 'K')))))
     ((make-printer 'O'))
      """)

  @Test def testMakePrinterComplex = compileAndInterpret("""
    (def make-printer (fun (x y)
                         (fun ()
                           (@byte-write (@char->int x))
                           (@byte-write (@char->int y)))))
    ((make-printer 'O' 'K'))
    """)

  @Test def testLetFun = compileAndInterpret("""
    (let* ((x 'O') (y 'K'))
          (letrec ((f (fun ()
                       (@byte-write (@char->int x))
                       (@byte-write (@char->int y)))))
                   (f)))
  """)

  @Test def testMakeAdder = compileAndInterpret("""
    (def make-adder
      (fun (x)
       (fun (y) (@+ x y))))
    (def increment (make-adder 1))

    (if (@= (increment 41) 42)
          (begin (@byte-write (@char->int 'O'))
           (@byte-write (@char->int 'K'))))
  """)

  @Test def testComposeFuns = compileAndInterpret("""
     (def compose
        (fun (f g)
          (fun (x) (f (g x)))))

     (def square (fun (x) (@* x x)))

     (if (@=((compose square square) 5) 625)
           (begin
            (@byte-write (@char->int 'O'))
            (@byte-write (@char->int 'K'))))
  """)
}

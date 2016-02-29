package l3.test

import infrastructure.L3Test
import org.junit.Test

/** Whitebox testing for individual transformation rules */
class L3DesugaringWhitebox extends L3Test {

  @Test def testSBegin1 = testL3TreeEquality(
    "(begin #u)",
    "#u"
  )

  @Test def testSBegin2 = testL3TreeEquality(
    "(begin 1 2 #u)",
    "(let ((v$1 1)) (let ((v$2 2)) #u))"
   )

  @Test def testSFun1 = testL3TreeEquality(
    "(fun (x) x)",
    "(letrec ((v$1 (fun (v$2) v$2))) v$1)"
  )

  @Test def testSFun2 = testL3TreeEquality(
    "(fun (x y z) x y z)",
    "(letrec ((v$1 (fun (v$2 v$3 v$4) (let ((v$5 v$2)) (let ((v$6 v$3)) v$4))))) v$1)"
  )

  @Test def testSLetStar1 = testL3TreeEquality(
    "(let* ((x 1)) #u)",
    "(let ((v$1 1)) #u)"
  )

  @Test def testSLetStar2 = testL3TreeEquality(
    "(let* ((x 1) (x 2)) x)",
    "(let ((v$1 1)) (let ((v$2 2)) v$2))"
  )

  @Test def testSLetStar3 = testL3TreeEquality(
    "(let* ((x 1) (x 2)) x 3)",
    "(let ((v$1 1)) (let ((v$2 2)) (let ((v$3 v$2)) 3)))"
  )

  @Test def testSRec1 = testL3TreeEquality(
    "(rec n ((n1 1)) #u)",
    "(letrec ((v$1 (fun (v$2) #u))) (v$1 1))"
  )

  @Test def testSRec2 = testL3TreeEquality(
    "(rec n ((n1 1) (n2 2)) n1)",
    "(letrec ((v$1 (fun (v$2 v$3) v$2))) (v$1 1 2))"
  )

  @Test def testSRec3 = testL3TreeEquality(
    "(rec n ((n1 1) (n2 2)) n1 n2)",
    "(letrec ((v$1 (fun (v$2 v$3) (let ((v$4 v$2)) v$3)))) (v$1 1 2))"
  )

  @Test def testSCond1 = testL3TreeEquality(
    "(cond (#t 2))",
    "(if #t 2 #u)"
  )

  @Test def testSCond2 = testL3TreeEquality(
    "(cond (#f 2) (#t 3))",
    "(if #f 2 (if #t 3 #u))"
  )

  @Test def testSAnd = testL3TreeEquality(
    "(fun (x y) (and x y))",
    "(letrec ((v$1 (fun (v$2 v$3) (if v$2 v$3 #f)))) v$1)"
  )

  @Test def testSOr = testL3TreeEquality(
    "(fun (x y) (or x y))",
    "(letrec ((v$1 (fun (v$2 v$3) (let ((v$4 v$2)) (if v$4 v$4 v$3))))) v$1)"
  )

  @Test def testSNot = testL3TreeEquality(
    "(fun (x) (not x))",
    "(letrec ((v$1 (fun (v$2) (if v$2 #f #t)))) v$1)"
  )
}

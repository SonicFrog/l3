package l3.test.infrastructure

import java.security._
import scala.runtime.ScalaRunTime
import java.io._

/**
 * Support for running tests in protected mode, so they don't trash our machines
 * (shamelessly stolen from Lukas Rytz's grading scripts for "Functional Programming in Scala")
 *
 * @author Lukas Rytz <lukas.rytz@epfl.ch>
 * @author Vlad Ureche <vlad.ureche@epfl.ch>
 */
class SandboxedTest {

  // force loading ScalaRunTime, which cannot load during the test due to
  // the security policy in place, that prevents reflection
  ScalaRunTime
  compat.Platform

  /** Run test in a restricted environment, where the code is not allowed to mess up the machine */
  def sandboxedTest[T](test: => T) = {
    val action = new PrivilegedAction[T] {
      def run: T = { test }
    }
    val originalContext = AccessController.getContext
    val combiner = new DomainCombiner {
      def combine(p1: Array[ProtectionDomain], p2: Array[ProtectionDomain]): Array[ProtectionDomain] = {
        // revoke all permissions
        Array(new ProtectionDomain(null, new Permissions()))
      }
    }
    val cntext = new AccessControlContext(originalContext, combiner)
    AccessController.doPrivileged(action, cntext)
  }

  def sandboxedTestWithRedirectedIO[T](test: => T, input: String): String = {
    val old_in = System.in
    val new_in = new BufferedInputStream(new ByteArrayInputStream(input.getBytes("UTF-8")))
    val old_out = System.out
    val output = new ByteArrayOutputStream()
    val new_out = new PrintStream(output)
    System.setIn(new_in)
    System.setOut(new_out)
    sandboxedTest(test)
    System.setOut(old_out)
    System.setIn(old_in)
    output.toString
  }
}

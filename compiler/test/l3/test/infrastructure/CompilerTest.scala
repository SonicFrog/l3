package l3.test.infrastructure

import l3.{L3FatalError, Position, L3Parser, Formatter}
import fastparse.core.Parsed._

/**
 * Common compiler testing infrastructure
 *
 * @author Vlad Ureche <vlad.ureche@epfl.ch>
 */
trait CompilerTest extends SandboxedTest {

  /** Compile the given source using a customizable set of phases given in pipeline */
  private def compileInner[T](source: () => String, pipeline: () => (l3.NominalCL3TreeModule.Tree => T)): T = {

    val dummyPosition: Int => Position = _ => new Position("foo", 0, 0)

    L3Parser.parse(source(), dummyPosition) match {
      case Success(tree, _) =>
        pipeline()(tree)
      case Failure(lp, index, _) =>
        assert(false, s"parse error (expected: $lp)")
        ???
    }
  }

  def compileUsingPipeline[T](source: () => String, pipeline: () => (l3.NominalCL3TreeModule.Tree => T)): T =
    sandboxedTest { compileInner(source, pipeline) }

  def compileUsingPipelineAndRedirect[T](source: () => String, pipeline: () => (l3.NominalCL3TreeModule.Tree => T), input: String): String =
    sandboxedTestWithRedirectedIO(compileInner(source, pipeline), input)

  def TreeToString[T](implicit f: Formatter[T]): TreeToString[T] = new TreeToString[T]

  /** A compiler phase that stores the tree as a string */
  class TreeToString[T](implicit f: Formatter[T]) extends Function1[T, String] {
    def apply(t: T): String = {
      val output = new java.io.StringWriter()
      f.toDocument(t).format(78, output)
      output.toString
    }
  }
}

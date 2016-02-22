package l3.test.infrastructure

/**
 * Basic testing functionality
 *
 * @author Vlad Ureche <vlad.ureche@epfl.ch>
 */
trait BasicTest {
  /** A method that checks string equality */
  def assertEqual(source: String, input: String, output: String, expected: String) = {
    if (output.replaceAll("\\s+", "") != expected.replaceAll("\\s+", "")) {
      val sb = new StringBuffer()
      sb.append("\nOutput is different: (whitespace is always ignored)")
      sb.append("\nsource: \n")
      sb.append(source)
      if (input != "") {
        sb.append("\ninput: \n")
        sb.append(input)
      }
      sb.append("\noutput: \n")
      sb.append(output)
      sb.append("\nexpected output: \n")
      sb.append(expected)
      assert(false, sb.toString)
    }
  }
}

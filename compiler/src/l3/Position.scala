package l3

final class Position(val fileName: String, val line: Int, val column: Int) {
  override def equals(thatO: Any): Boolean =
    thatO.isInstanceOf[Position] && {
      val that = thatO.asInstanceOf[Position]
      fileName == that.fileName && line == that.line && column == that.column
    }

  override def toString: String =
    fileName + ":" + line + ":" + column
}

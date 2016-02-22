package l3

/**
 * Error-reporting module for the L₃ compiler.
 *
 * @author Michel Schinz <Michel.Schinz@epfl.ch>
 */

object Reporter {
  def fatalError(pos: Position, msg: String): Nothing = {
    throw L3FatalError(pos + ": "+ msg)

  }
}

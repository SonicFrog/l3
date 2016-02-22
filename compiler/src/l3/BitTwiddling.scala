package l3

/**
 * Bit twiddling utility module.
 * Parts taken from http://graphics.stanford.edu/~seander/bithacks.html
 *
 * @author Michel Schinz <Michel.Schinz@epfl.ch>
 */

object BitTwiddling {
  def bitsToIntMSBF(bits: Int*): Int =
    (0 /: bits){ (v, b) => (v << 1) | b }

  def fitsInNSignedBits(bits: Int)(value: Int): Boolean = {
    require(0 <= bits && bits < 32)
    val value1 = value >> (bits - 1)
    value1 == 0 || value1 == -1
  }

  def fitsInNUnsignedBits(bits: Int)(value: Int): Boolean = {
    require(0 <= bits && bits < 32)
    0 <= value && value < (1 << bits)
  }
}

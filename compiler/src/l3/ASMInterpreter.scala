package l3

import java.lang.Math.{ floorDiv, floorMod }
import PCRelativeASMInstructionModule._
import IO._

/**
 * An interpreter for the ASM language.
 *
 * @author Michel Schinz <Michel.Schinz@epfl.ch>
 */

object ASMInterpreter extends (Seq[Instruction] => Unit) {
  def apply(program: Seq[Instruction]): Unit =
    interpret(program.toArray)

  private def interpret(program: Array[Instruction]): Unit = {
    import scala.language.implicitConversions

    var PC: Int = 0

    def error(msg: String): Nothing = {
      throw L3FatalError(msg +" (at PC = "+ PC +")")
    }

    implicit def intToValue(i: Int): Value = IntV(i)
    implicit def valueToInt(v: Value): Int = v match {
      case IntV(i) => i
      case BlockV(a, _, _) => a
      case _ => error("expected integer, found "+ v)
    }
    implicit def valueToBlock(v: Value): BlockV = v match {
      case b: BlockV => b
      case _ => error("expected block, found "+ v)
    }

    trait Value
    case class IntV(value: Int) extends Value
    case class BlockV(addr: Int, tag: Int, contents: Array[Value]) extends Value
    case object UndefV extends Value

    var nextBlockAddr = 0
    def allocBlock(tag: Int, size: Int): BlockV = {
      val block = BlockV(nextBlockAddr, tag, Array.fill(size)(UndefV))
      nextBlockAddr += 4
      block
    }

    import ASMRegisterFile.{ Ib, Lb, Ob }
    val I0 = ASMRegisterFile.in(0)
    val I1 = ASMRegisterFile.in(1)
    val I2 = ASMRegisterFile.in(2)
    val I3 = ASMRegisterFile.in(3)
    val I4 = ASMRegisterFile.in(4)
    val O0 = ASMRegisterFile.out(0)
    val O1 = ASMRegisterFile.out(1)
    val O2 = ASMRegisterFile.out(2)
    val O3 = ASMRegisterFile.out(3)

    object R {
      private var Ib: Value = UndefV
      private var Lb: Value = UndefV
      private var Ob: Value = UndefV

      private def checkedContents(r: ASMRegister): Array[Value] = {
        val contents = this(r.base).contents
        if (0 <= r.index && r.index <= contents.length)
          contents
        else
          error("register not mapped: "+ r)
      }

      def apply(reg: ASMBaseRegister): Value = reg match {
        case ASMRegisterFile.Ib => Ib
        case ASMRegisterFile.Lb => Lb
        case ASMRegisterFile.Ob => Ob
      }

      def apply(reg: ASMRegister): Value =
        checkedContents(reg)(reg.index)

      def update(reg: ASMRegister, newValue: Value): Unit =
        checkedContents(reg)(reg.index) = newValue

      def update(reg: ASMBaseRegister, newValue: Value): Unit = reg match {
        case ASMRegisterFile.Ib => Ib = newValue
        case ASMRegisterFile.Lb => Lb = newValue
        case ASMRegisterFile.Ob => Ob = newValue
      }
    }

    var done = false
    while (!done) {
      program(PC) match {
        case ADD(a, b, c) =>
          R(a) = R(b) + R(c)
          PC += 1

        case SUB(a, b, c) =>
          R(a) = R(b) - R(c)
          PC += 1

        case MUL(a, b, c) =>
          R(a) = R(b) * R(c)
          PC += 1

        case DIV(a, b, c) =>
          R(a) = floorDiv(R(b), R(c))
          PC += 1

        case MOD(a, b, c) =>
          R(a) = floorMod(R(b), R(c))
          PC += 1

        case ASL(a, b, c) =>
          R(a) = R(b) << R(c)
          PC += 1

        case ASR(a, b, c) =>
          R(a) = R(b) >> R(c)
          PC += 1

        case AND(a, b, c) =>
          R(a) = R(b) & R(c)
          PC += 1

        case OR(a, b, c) =>
          R(a) = R(b) | R(c)
          PC += 1

        case XOR(a, b, c) =>
          R(a) = R(b) ^ R(c)
          PC += 1

        case JLT(a, b, d) =>
          PC += (if (R(a) < R(b)) d else 1)

        case JLE(a, b, d) =>
          PC += (if (R(a) <= R(b)) d else 1)

        case JEQ(a, b, d) =>
          PC += (if (R(a) == R(b)) d else 1)

        case JNE(a, b, d) =>
          PC += (if (R(a) != R(b)) d else 1)

        case JGE(a, b, d) =>
          PC += (if (R(a) >= R(b)) d else 1)

        case JGT(a, b, d) =>
          PC += (if (R(a) > R(b)) d else 1)

        case JI(d) =>
          PC += d

        case TCAL(a) =>
          val targetPC = R(a) >> 2
          // copy caller state (Ib, Lb, Ob and return address)
          R(O0) = R(I0)
          R(O1) = R(I1)
          R(O2) = R(I2)
          R(O3) = R(I3)
          // initialize callee state (Ib, Lb, Ob and PC)
          R(Ib) = R(Ob)
          R(Lb) = UndefV
          R(Ob) = UndefV
          PC = targetPC

        case CALL(a) =>
          val targetPC = R(a) >> 2
          // save caller state (Ib, Lb, Ob and return address)
          R(O0) = R(Ib)
          R(O1) = R(Lb)
          R(O2) = R(Ob)
          R(O3) = PC + 1
          // initialize callee state (Ib, Lb, Ob and PC)
          R(Ib) = R(Ob)
          R(Lb) = UndefV
          R(Ob) = UndefV
          PC = targetPC

        case RET =>
          // restore caller state (Ib, Lb, Ob and PC)
          val retValue = R(I4)
          PC = R(I3)
          R(Ob) = R(I2)
          R(Lb) = R(I1)
          R(Ib) = R(I0)
          R(O0) = retValue

        case HALT(_) =>
          done = true

        case LDLO(a, s) =>
          assume(-0x20000 <= s && s < 0x20000)
          R(a) = s
          PC += 1

        case LDHI(a, u) =>
          R(a) = (u << 16) | (R(a) & 0xFFFF)
          PC += 1

        case MOVE(a, b) =>
          R(a) = R(b)
          PC += 1

        case RALO(a, s) =>
          R(a) = allocBlock(BlockTag.RegisterFrame.id, s)
          PC += 1

        case BALO(a, b, t) =>
          R(a) = allocBlock(t, R(b))
          PC += 1

        case BSIZ(a, b) =>
          R(a) = R(b).contents.length
          PC += 1

        case BTAG(a, b) =>
          R(a) = R(b).tag
          PC += 1

        case BGET(a, b, c) =>
          R(a) = R(b).contents(R(c))
          PC += 1

        case BSET(a, b, c) =>
          R(b).contents(R(c)) = R(a)
          PC += 1

        case BREA(a) =>
          R(a) = readByte()
          PC += 1

        case BWRI(a) =>
          writeByte(R(a))
          PC += 1
      }
    }
  }
}

package l3

import java.lang.Math.{ floorDiv, floorMod }
import scala.collection.mutable.{ Map => MutableMap }
import SymbolicCL3TreeModule._
import IO._

/**
 * A tree-based interpreter for the CL₃ language.
 *
 * @author Michel Schinz <Michel.Schinz@epfl.ch>
 */

object CL3Interpreter extends (Tree => Unit) {
  def apply(program: Tree): Unit =
    try {
      eval(program)(Map.empty)
    } catch {
      case e: EvalError =>
        for (msgs <- e.messages; msg <- msgs.reverseIterator)
          println(msg)
    }

  // Values
  private sealed trait Value {
    override def toString(): String = this match {
      case BlockV(t, c) => s"<$t>[${c mkString ","}]"
      case IntV(i) => i.toString
      case CharV(c) => s"'${new String(Array(c), 0, 1)}'"
      case BoolV(b) => if (b) "#t" else "#f"
      case UnitV => "#u"
      case FunctionV(_, _, _) => "<function>"
    }
  }
  private case class BlockV(tag: Int, contents: Array[Value]) extends Value
  private case class IntV_(i: Int) extends Value {
    require(BitTwiddling.fitsInNSignedBits(31)(i))
  }
  private case class CharV(c: Int) extends Value
  private case class BoolV(b: Boolean) extends Value
  private case object UnitV extends Value

  private case class FunctionV(args: Seq[Symbol], body: Tree, env: Env)
               extends Value

  // Make sure that integer values are 31 bits.
  private object IntV {
    def apply(v: Int): Value = IntV_((v << 1) >> 1)
    def unapply(v: Value): Option[Int] = v match {
      case IntV_(i) => Some(i)
      case _ => None
    }
  }

  // Environment
  private type Env = PartialFunction[Symbol, Value]

  // Error handling
  private class EvalError(val messages: Option[Seq[String]]) extends Exception()
  private def error(pos: Position, msg: String): Nothing =
    throw new EvalError(Some(Seq(s"Error: $msg", s"  at $pos")))
  private def halt(): Nothing =
    throw new EvalError(None)

  private def validIndex(a: Array[Value], i: Int): Boolean =
    0 <= i && i < a.length

  private final def eval(tree: Tree)(implicit env: Env): Value = tree match {
    case Let(bdgs, body) =>
      eval(body)(Map(bdgs map { case (n, e) => n -> eval(e) } : _*) orElse env)

    case LetRec(funs, body) =>
      val recEnv = MutableMap[Symbol, Value]()
      val env1 = recEnv orElse env
      for (FunDef(name, args, body) <- funs)
        recEnv(name) = BlockV(BlockTag.Function.id,
                              Array(FunctionV(args, body, env1)))
      eval(body)(env1)

    case If(cond, thenE, elseE) =>
      eval(cond) match {
        case BoolV(false) => eval(elseE)
        case _ => eval(thenE)
      }

    case App(fun, args) =>
      eval(fun) match {
        case BlockV(_, Array(FunctionV(cArgs, cBody, cEnv))) =>
          if (args.length != cArgs.length)
            error(tree.pos,
                  s"expected ${cArgs.length} arguments, got ${args.length}")
          try {
            eval(cBody)(Map(cArgs zip (args map eval) : _*) orElse cEnv)
          } catch {
            case e: EvalError =>
              throw new EvalError(e.messages map (_ :+ s"  at ${fun.pos}"))
          }
        case _ => error(fun.pos, "function value expected")
      }

    case Prim(p, args) => (p, args map eval) match {
      case (L3BlockAlloc(t), Seq(IntV(i))) => BlockV(t, Array.fill(i)(UnitV))
      case (L3BlockP, Seq(BlockV(_, _))) => BoolV(true)
      case (L3BlockP, Seq(_)) => BoolV(false)
      case (L3BlockTag, Seq(BlockV(t, _))) => IntV(t)
      case (L3BlockLength, Seq(BlockV(_, c))) => IntV(c.length)
      case (L3BlockGet, Seq(BlockV(_, v), IntV(i))) if (validIndex(v, i)) =>
        v(i)
      case (L3BlockSet, Seq(BlockV(_, v), IntV(i), o)) if (validIndex(v, i)) =>
        v(i) = o; UnitV

      case (L3IntP, Seq(IntV(_))) => BoolV(true)
      case (L3IntP, Seq(_)) => BoolV(false)

      case (L3IntAdd, Seq(IntV(v1), IntV(v2))) => IntV(v1 + v2)
      case (L3IntSub, Seq(IntV(v1), IntV(v2))) => IntV(v1 - v2)
      case (L3IntMul, Seq(IntV(v1), IntV(v2))) => IntV(v1 * v2)
      case (L3IntDiv, Seq(IntV(v1), IntV(v2))) => IntV(floorDiv(v1, v2))
      case (L3IntMod, Seq(IntV(v1), IntV(v2))) => IntV(floorMod(v1, v2))

      case (L3IntArithShiftLeft, Seq(IntV(v1), IntV(v2))) => IntV(v1 << v2)
      case (L3IntArithShiftRight, Seq(IntV(v1), IntV(v2))) => IntV(v1 >> v2)
      case (L3IntBitwiseAnd, Seq(IntV(v1), IntV(v2))) => IntV(v1 & v2)
      case (L3IntBitwiseOr, Seq(IntV(v1), IntV(v2))) => IntV(v1 | v2)
      case (L3IntBitwiseXOr, Seq(IntV(v1), IntV(v2))) => IntV(v1 ^ v2)

      case (L3IntLt, Seq(IntV(v1), IntV(v2))) => BoolV(v1 < v2)
      case (L3IntLe, Seq(IntV(v1), IntV(v2))) => BoolV(v1 <= v2)
      case (L3Eq, Seq(v1, v2)) => BoolV(v1 == v2)
      case (L3Ne, Seq(v1, v2)) => BoolV(v1 != v2)
      case (L3IntGe, Seq(IntV(v1), IntV(v2))) => BoolV(v1 >= v2)
      case (L3IntGt, Seq(IntV(v1), IntV(v2))) => BoolV(v1 > v2)

      case (L3IntToChar, Seq(IntV(i)))
          if Character.isValidCodePoint(i) => CharV(i)

      case (L3CharP, Seq(CharV(_))) => BoolV(true)
      case (L3CharP, Seq(_)) => BoolV(false)

      case (L3ByteRead, Seq()) => IntV(readByte())
      case (L3ByteWrite, Seq(IntV(c))) => writeByte(c); UnitV

      case (L3CharToInt, Seq(CharV(c))) => IntV(c)

      case (L3BoolP, Seq(BoolV(_))) => BoolV(true)
      case (L3BoolP, Seq(_)) => BoolV(false)

      case (L3UnitP, Seq(UnitV)) => BoolV(true)
      case (L3UnitP, Seq(_)) => BoolV(false)

      case (p, vs) =>
        error(tree.pos,
              s"""cannot apply primitive $p to values ${vs.mkString(", ")}""")
    }

    case Halt(arg) => eval(arg) match {
      case IntV(0) => halt()
      case c => error(tree.pos, s"halt with code $c")
    }

    case Ident(n) => env(n)

    case Lit(IntLit(i)) => IntV(i)
    case Lit(CharLit(c)) => CharV(c)
    case Lit(BooleanLit(b)) => BoolV(b)
    case Lit(UnitLit) => UnitV
  }
}

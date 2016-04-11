package l3

import java.lang.Math.{ floorDiv, floorMod }
import scala.annotation.tailrec
import scala.collection.mutable.{ Map => MutableMap }
import IO._

/**
 * A tree-based interpreter for the CPS languages.
 *
 * @author Michel Schinz <Michel.Schinz@epfl.ch>
 */

abstract class CPSInterpreter[M <: CPSTreeModule](protected val treeModule: M) {
  import treeModule._

  def apply(tree: Tree): Unit =
    eval(tree, emptyEnv)

  def statistics(tree: Tree): Unit =
    ()

  protected sealed trait Value
  protected case class FunV(retC: Name, args: Seq[Name], body: Tree, env: Env)
      extends Value
  protected case class CntV(args: Seq[Name], body: Tree, env: Env)
      extends Value

  protected type Env = PartialFunction[Name, Value]
  protected val emptyEnv: Env = Map.empty

  @tailrec
  private def eval(tree: Tree, env: Env): Unit = {
    statistics(tree: Tree)
    (tree: @unchecked) match {
      case LetL(name, lit, body) =>
        eval(body, Map(name -> evalLit(lit)) orElse env)

      case LetP(name, prim, args, body) =>
        eval(body, Map(name -> evalValuePrim(prim, args map env)) orElse env)

      case LetC(cnts, body) =>
        val recEnv = MutableMap[Name, Value]()
        val env1 = recEnv orElse env
        for (CntDef(name, args, body) <- cnts)
          recEnv(name) = CntV(args, body, env1)
        eval(body, env1)

      case LetF(funs, body) =>
        val recEnv = MutableMap[Name, Value]()
        val env1 = recEnv orElse env
        for (FunDef(name, retC, args, body) <- funs)
          recEnv(name) = wrapFunV(FunV(retC, args, body, env1))
        eval(body, env1)

      case AppC(cnt, args) =>
        val CntV(cArgs, cBody, cEnv) = env(cnt)
        assume(cArgs.length == args.length)
        eval(cBody, Map(cArgs zip (args map env) : _*) orElse cEnv)

      case AppF(fun, retC, args) =>
        val FunV(fRetC, fArgs, fBody, fEnv) = unwrapFunV(env(fun))
        assume(fArgs.length == args.length)
        eval(fBody,
             Map((fRetC +: fArgs) zip ((retC +: args) map env): _*) orElse fEnv)

      case If(cond, args, thenC, elseC) =>
        val cnt = if (evalTestPrim(cond, args map env)) thenC else elseC
        val cntV = env(cnt).asInstanceOf[CntV]
        eval(cntV.body, cntV.env)

      case Halt(_) =>
        ()
    }
  }

  protected def wrapFunV(funV: FunV): Value
  protected def unwrapFunV(v: Value): FunV

  protected def evalLit(l: Literal): Value
  protected def evalValuePrim(p: ValuePrimitive, args: Seq[Value]): Value
  protected def evalTestPrim(p: TestPrimitive, args: Seq[Value]): Boolean
}

object CPSInterpreterHigh extends CPSInterpreterHigh
class CPSInterpreterHigh extends CPSInterpreter(SymbolicCPSTreeModule)
    with (SymbolicCPSTreeModule.Tree => Unit) {
  import treeModule._

  private case class BlockV(tag: Int, contents: Array[Value]) extends Value
  private case class IntV_(value: Int) extends Value {
    require(BitTwiddling.fitsInNSignedBits(31)(value))
  }
  private case class CharV(value: Int) extends Value
  private case class BooleanV(value: Boolean) extends Value
  private case object UnitV extends Value

  private object IntV {
    def apply(v: Int): Value = IntV_((v << 1) >> 1)
    def unapply(v: Value): Option[Int] = v match {
      case IntV_(i) => Some(i)
      case _ => None
    }
  }

  protected def wrapFunV(funV: FunV): Value =
    BlockV(BlockTag.Function.id, Array(funV))
  protected def unwrapFunV(v: Value): FunV = v match {
    case BlockV(_, Array(funV: FunV)) => funV
  }

  protected def evalLit(l: Literal): Value = l match {
    case IntLit(i) => IntV(i)
    case CharLit(c) => CharV(c)
    case BooleanLit(b) => BooleanV(b)
    case UnitLit => UnitV
  }

  protected def evalValuePrim(p: ValuePrimitive, args: Seq[Value]): Value =
    (p, args) match {
      case (L3BlockAlloc(t), Seq(IntV(i))) => BlockV(t, Array.fill(i)(UnitV))
      case (L3BlockTag, Seq(BlockV(t, _))) => IntV(t)
      case (L3BlockLength, Seq(BlockV(_, c))) => IntV(c.length)
      case (L3BlockGet, Seq(BlockV(_, v), IntV(i))) => v(i)
      case (L3BlockSet, Seq(BlockV(_, v), IntV(i), o)) => v(i) = o; UnitV

      case (L3IntAdd, Seq(IntV(v1), IntV(v2))) => IntV(v1 + v2)
      case (L3IntSub, Seq(IntV(v1), IntV(v2))) => IntV(v1 - v2)
      case (L3IntMul, Seq(IntV(v1), IntV(v2))) => IntV(v1 * v2)
      case (L3IntDiv, Seq(IntV(v1), IntV(v2))) => IntV(floorDiv(v1, v2))
      case (L3IntMod, Seq(IntV(v1), IntV(v2))) => IntV(floorMod(v1, v2))
      case (L3IntToChar, Seq(IntV(v))) => CharV(v)

      case (L3IntArithShiftLeft, Seq(IntV(v1), IntV(v2))) => IntV(v1 << v2)
      case (L3IntArithShiftRight, Seq(IntV(v1), IntV(v2))) => IntV(v1 >> v2)
      case (L3IntBitwiseAnd, Seq(IntV(v1), IntV(v2))) => IntV(v1 & v2)
      case (L3IntBitwiseOr, Seq(IntV(v1), IntV(v2))) => IntV(v1 | v2)
      case (L3IntBitwiseXOr, Seq(IntV(v1), IntV(v2))) => IntV(v1 ^ v2)

      case (L3ByteRead, Seq()) => IntV(readByte())
      case (L3ByteWrite, Seq(IntV(c))) => writeByte(c); UnitV
      case (L3CharToInt, Seq(CharV(c))) => IntV(c.toInt)

      case (L3Id, Seq(v)) => v
    }

  protected def evalTestPrim(p: TestPrimitive, args: Seq[Value]): Boolean =
    (p, args) match {
      case (L3BlockP, Seq(BlockV(_, _))) => true
      case (L3BlockP, Seq(_)) => false

      case (L3IntP, Seq(IntV(_))) => true
      case (L3IntP, Seq(_)) => false
      case (L3IntLt, Seq(IntV(v1), IntV(v2))) => v1 < v2
      case (L3IntLe, Seq(IntV(v1), IntV(v2))) => v1 <= v2
      case (L3IntGe, Seq(IntV(v1), IntV(v2))) => v1 >= v2
      case (L3IntGt, Seq(IntV(v1), IntV(v2))) => v1 > v2

      case (L3CharP, Seq(CharV(_))) => true
      case (L3CharP, Seq(_)) => false

      case (L3BoolP, Seq(BooleanV(_))) => true
      case (L3BoolP, Seq(_)) => false

      case (L3UnitP, Seq(UnitV)) => true
      case (L3UnitP, Seq(_)) => false

      case (L3Eq, Seq(v1, v2)) => v1 == v2
      case (L3Ne, Seq(v1, v2)) => v1 != v2
    }
}

object CPSInterpreterLow extends CPSInterpreterLow
class CPSInterpreterLow extends CPSInterpreter(SymbolicCPSTreeModuleLow)
    with (SymbolicCPSTreeModuleLow.Tree => Unit) {
  import treeModule._
  import scala.language.implicitConversions

  protected case class BlockV(addr: Int, tag: Int, contents: Array[Value])
      extends Value
  protected case class IntV(value: Int) extends Value

  private var nextBlockAddr = 0
  private def allocBlock(tag: Int, contents: Array[Value]): BlockV = {
    val block = BlockV(nextBlockAddr, tag, contents)
    nextBlockAddr += 4
    block
  }

  private implicit def valueToInt(v: Value): Int = v match {
    case BlockV(addr, _, _) => addr
    case IntV(value)        => value
    case _: FunV | _: CntV  => sys.error(s"cannot convert $v to integer")
  }

  protected def wrapFunV(funV: FunV): Value = funV
  protected def unwrapFunV(v: Value): FunV = v.asInstanceOf[FunV]

  protected def evalLit(l: Literal): Value = IntV(l)

  protected def evalValuePrim(p: ValuePrimitive, args: Seq[Value]): Value =
    (p, args) match {
      case (CPSAdd, Seq(v1, v2)) => IntV(v1 + v2)
      case (CPSSub, Seq(v1, v2)) => IntV(v1 - v2)
      case (CPSMul, Seq(v1, v2)) => IntV(v1 * v2)
      case (CPSDiv, Seq(v1, v2)) => IntV(floorDiv(v1, v2))
      case (CPSMod, Seq(v1, v2)) => IntV(floorMod(v1, v2))

      case (CPSArithShiftL, Seq(v1, v2)) => IntV(v1 << v2)
      case (CPSArithShiftR, Seq(v1, v2)) => IntV(v1 >> v2)
      case (CPSAnd, Seq(v1, v2)) => IntV(v1 & v2)
      case (CPSOr, Seq(v1, v2)) => IntV(v1 | v2)
      case (CPSXOr, Seq(v1, v2)) => IntV(v1 ^ v2)

      case (CPSByteRead, Seq()) => IntV(readByte())
      case (CPSByteWrite, Seq(c)) => writeByte(c); IntV(0)

      case (CPSBlockAlloc(t), Seq(s)) => allocBlock(t, Array.fill(s)(IntV(0)))
      case (CPSBlockTag, Seq(BlockV(_, t, _))) => IntV(t)
      case (CPSBlockLength, Seq(BlockV(_, _, c))) => IntV(c.length)
      case (CPSBlockGet, Seq(BlockV(_, _, c), i)) => c(i)
      case (CPSBlockSet, Seq(BlockV(_, _, c), i, v)) => c(i) = v; IntV(0)

      case (CPSId, Seq(o)) => o
    }

  protected def evalTestPrim(p: TestPrimitive, args: Seq[Value]): Boolean =
    (p, args) match {
      case (CPSLt, Seq(v1, v2)) => v1 < v2
      case (CPSLe, Seq(v1, v2)) => v1 <= v2
      case (CPSEq, Seq(v1, v2)) => v1 == v2
      case (CPSNe, Seq(v1, v2)) => v1 != v2
      case (CPSGe, Seq(v1, v2)) => v1 >= v2
      case (CPSGt, Seq(v1, v2)) => v1 > v2
    }
}

class CPSInterpreterLowWithStats(showStats: Boolean) extends CPSInterpreterLow {

  import SymbolicCPSTreeModuleLow._

  val stats = new Statistics()

  override def statistics(tree: Tree): Unit = {
    stats.inc(tree.getClass())
    tree match {
      case LetP(_, prim, _, _) =>
        stats.inc(prim.getClass())
      case LetF(funs, _) =>
        stats.incFuncs(funs.length)
      case LetC(conts, _) =>
        stats.incConts(conts.length)
      case If(cond, _, _, _) =>
        stats.inc(cond.getClass())
      case Halt(_) =>
        if (showStats) stats.print()
      case _ =>
    }
  }
}

class Statistics {
  import scala.collection.mutable.Map
  import Statistics._

  private[this] var funcs = 0
  private[this] var conts = 0
  private[this] val insts = Map[Class[_ <: l3.CPSTreeModule#Tree], Int]()
  private[this] val lprims = Map[Class[_ <: l3.CPSTestPrimitive], Int]()
  private[this] val vprims = Map[Class[_ <: l3.CPSValuePrimitive], Int]()
  private[this] def m_inc[T](map: Map[T, Int], cls: T): Unit = map += cls -> (m_get(map, cls) + 1)
  private[this] def m_get[T](map: Map[T, Int], cls: T): Int =  map.getOrElse(cls, 0)

  def inc(cls: Class[_ <: l3.CPSTreeModule#Tree])(implicit ov: OverloadHack1.type) = m_inc(insts, cls)
  def inc(cls: Class[_ <: l3.CPSTestPrimitive])(implicit ov: OverloadHack2.type)   = m_inc(lprims, cls)
  def inc(cls: Class[_ <: l3.CPSValuePrimitive])(implicit ov: OverloadHack3.type)  = m_inc(vprims, cls)
  def get(cls: Class[_ <: l3.CPSTreeModule#Tree])(implicit ov: OverloadHack1.type) = m_get(insts, cls)
  def get(cls: Class[_ <: l3.CPSTestPrimitive])(implicit ov: OverloadHack2.type)   = m_get(lprims, cls)
  def get(cls: Class[_ <: l3.CPSValuePrimitive])(implicit ov: OverloadHack3.type)  = m_get(vprims, cls)

  def incFuncs(incr: Int) = funcs += incr
  def incConts(incr: Int) = conts += incr
  def getFuncs = funcs
  def getConts = conts

  def print() =
    println(toString)

  override def toString: String = {
    def append(label: String, vals: Map[_ <: Class[_], Int], sb: StringBuffer) =
      if (!vals.isEmpty) {
        sb.append(label + "\n" + "=" * label.length + "\n")
        for ((occ, prim) <- vals.toList.map(p => (p._2, p._1.getSimpleName())).sortBy(-_._1))
          sb.append("%8d  %s\n".format(occ, prim))
        sb.append("\n")
      }

    // Create stats
    val sb = new StringBuffer()
    append("Instruction Stats", insts, sb)
    append("Value Primitives Stats", vprims, sb)
    append("Logic Primitives Stats", lprims, sb)

    sb.append("Functions defined: " + getFuncs + "\n")
    sb.append("Continuations defined: " + getConts + "\n")
    sb.toString
  }
}

object Statistics {
  /* We need to do overloading with methods that
   * have the exact same signature after erasure,
   * so we add an implicit OverloadHack parameter */
  implicit object OverloadHack1
  implicit object OverloadHack2
  implicit object OverloadHack3
}


package l3

import scala.collection.mutable.{ Map => MutableMap }

abstract class CPSOptimizer[T <: CPSTreeModule { type Name = Symbol }]
  (val treeModule: T) {
  import treeModule._

  def apply(tree: Tree): Tree = {
    val simplifiedTree = fixedPoint(tree)(shrink)
    val maxSize = (size(simplifiedTree) * 1.5).toInt
    val t = fixedPoint(simplifiedTree, 8) { t => inline(t, maxSize) }
    t
  }

  /* Counts how many times a symbol is encountered as an applied function,
   * and how many as a value
   */
  private case class Count(applied: Int = 0, asValue: Int = 0)

  /* Local state of the optimization
   * Note: To update the state, use the with* methods
   */
  private case class State(
    /* How many times a symbol is encountered in the Tree.
     * Note: The census for the whole program gets calculated once in the beginning,
     *       and passed to the initial state.
     */
    census: Map[Name, Count],
    // Name substitution that needs to be applied to the current tree
    subst: Substitution[Name] = Substitution.empty,
    // Names that have a constant value
    lEnv: Map[Name, Literal] = Map.empty,
    // The inverse of lEnv
    lInvEnv: Map[Literal, Name] = Map.empty,
    // A known block mapped to its tag and length
    bEnv: Map[Name, (Literal, Name)] = Map.empty,
    // ((p, args) -> n2) is included in eInvEnv iff n2 == p(args)
    // Note: useful for common-subexpression elimination
    eInvEnv: Map[(ValuePrimitive, Seq[Name]), Name] = Map.empty,
    // Continuations that will be inlined
    cEnv: Map[Name, CntDef] = Map.empty,
    // Functions that will be inlined
    fEnv: Map[Name, FunDef] = Map.empty) {

    // Checks whether a symbol is dead in the current state
    def dead(s: Name): Boolean =
      census get s map (_ == Count(applied = 0, asValue = 0)) getOrElse true
    // Checks whether a symbols is applied exactly once as a function in the current State,
    // and never used as a value
    def appliedOnce(s: Name): Boolean =
      census get s map (_ == Count(applied = 1, asValue = 0)) getOrElse false

    // Addas a substitution to the state
    def withSubst(from: Name, to: Name): State =
      copy(subst = subst + (from -> to))
    // Adds a Seq of substitutions to the state
    def withSubst(from: Seq[Name], to: Seq[Name]): State =
      copy(subst = subst ++ (from zip to))

    // Adds a constant to the State
    def withLit(name: Name, value: Literal) =
      copy(lEnv = lEnv + (name -> value), lInvEnv = lInvEnv + (value -> name))
    // Adds a block to the state
    def withBlock(name: Name, tag: Literal, size: Name) =
      copy(bEnv = bEnv + (name -> (tag, size)))
    // Adds a primitive assignment to the state
    def withExp(name: Name, prim: ValuePrimitive, args: Seq[Name]) =
      copy(eInvEnv = eInvEnv + ((prim, args) -> name))
    // Adds an inlinable continuation to the state
    def withCnt(cnt: CntDef) =
      copy(cEnv = cEnv + (cnt.name -> cnt))
    // Adds a Seq of inlinable continuations to the state
    def withCnts(cnts: Seq[CntDef]) =
      (this /: cnts) (_.withCnt(_))
    // Adds an inlinable function to the state
    def withFun(fun: FunDef) =
      copy(fEnv = fEnv + (fun.name -> fun))
    // Adds a Seq of inlinable functions to the state
    def withFuns(funs: Seq[FunDef]) =
      (this /: funs) (_.withFun(_))
    /*
     * The same state, with emply inverse environments.
     * Use this when entering a new FunDef, because assigned Name's may
     * come out of scope during hoisting.
     */
    def withEmptyInvEnvs =
      copy(lInvEnv = Map.empty, eInvEnv = Map.empty)
  }


  private def fvOf(tree : Tree) : Seq[Name] = {
    def F(tree : Tree) : Set[Name] = tree match {
      case LetL(name, _, e) => F(e) - name
      case LetP(name, _, args, e) => (F(e) - name) union args.toSet
      case LetC(cnts, e) => F(e) union (cnts.map(contFV).fold(Set())(_ union _))
      case LetF(funs, e) => (F(e) union (funs.map(funFV).fold(Set())(_ union _))) -- funs.map(_.name)
      case AppC(_, args) => args.toSet
      case AppF(funName, _, args) => args.toSet + funName
      case If(_, args, _, _) => args.toSet
      case Halt(arg) => Set(arg)
    }

    def funFV(f : FunDef) : Set[Name] = f match {
      case FunDef(_, _, args, e) => F(e) &~ args.toSet
    }

    def contFV(c : CntDef) : Set[Name] = c match {
      case CntDef(_, args, e) => F(e) &~ args.toSet
    }

    F(tree).toSeq
  }

  private def funFV(fun : FunDef) : Seq[Name] = {
    (fvOf(fun.body).toSet -- fun.args).toSeq
  }

  // Shrinking optimizations

  private def shrink(tree: Tree): Tree = {
    def isRightAbsorbing(x : Name, prim : ValuePrimitive)(implicit s : State) =
      s.lEnv.get(x).exists((a : Literal) => rightAbsorbing(prim -> a))
    def isLeftAbsorbing(x : Name, prim : ValuePrimitive)(implicit s : State) =
      s.lEnv.get(x).exists(a => leftAbsorbing(a -> prim))
    def isLeftNeutral(x : Name, prim : ValuePrimitive)(implicit s : State) =
      s.lEnv.get(x).exists(a => leftNeutral(a -> prim))
    def isRightNeutral(x : Name, prim : ValuePrimitive)(implicit s : State) =
      s.lEnv.get(x).exists(a => rightNeutral(prim -> a))

    def shrinkT(tree: Tree)(implicit s: State): Tree = {
      val t = tree match {
        // Dead code elimination
        case LetL(name, _, body) if s dead name => shrinkT(body)

        // Common sub-expr elimination
        case LetL(name, value, body) if s.lInvEnv contains value =>
          shrinkT(body)(s.withSubst(name, s.lInvEnv(value)))

        case LetL(name, value, body) => shrinkT(body)(s.withLit(name, value))

        // Neutral left element optimization
        case LetP(name, prim, Seq(x, y), body) if isLeftNeutral(x, prim) =>
          shrinkT(body)(s.withSubst(name, y))

        // Absorbing left element optimization
        case LetP(name, prim, Seq(x, y), body) if isLeftAbsorbing(x, prim) =>
          shrinkT(body)(s.withSubst(name, x))

        // Neutral right element
        case LetP(name, prim, Seq(x, y), body) if isRightNeutral(y, prim) =>
          shrinkT(body)(s.withSubst(name, x))

        // Absorbing right element
        case LetP(name, prim, Seq(x, y), body) if isRightAbsorbing(y, prim) =>
          shrinkT(body)(s.withSubst(name, y))

        // Common subexpr elimination
        case LetP(name, prim, args, body) if s.eInvEnv contains (prim, args) =>
          shrinkT(body)(s withSubst(name, s.eInvEnv(prim, args)))

        // Constant folding
        case LetP(name, prim, args, body) =>
          val argsValues = args flatMap s.lEnv.get

          if (vEvaluator.isDefinedAt(prim, argsValues))
            shrinkT(LetL(name, vEvaluator(prim, argsValues), body))
          else LetP(name, prim, args, shrinkT(body)(s withExp (name, prim, args)))

        // Removing dead continuations
        case LetC(cnts, body) if cnts exists (s dead _.name) =>
          val used = cnts.filter(c => !(s.dead(c.name)))
          shrinkT(LetC(used, body))

        // Inlining continuations
        case LetC(cnts, body) if cnts.exists(s appliedOnce _.name) =>
          val inlined = cnts.filter(c => s.appliedOnce(c.name))
          val notInlined = cnts diff inlined

          shrinkT(LetC(notInlined, body))(s.withCnts(inlined))

        case LetC(cnts, body) =>
          val continuations = cnts map { c =>
            val CntDef(name, args, body) = c
            CntDef(name, args, shrinkT(body))
          }

          if (continuations.isEmpty) shrinkT(body)
          else LetC(continuations, shrinkT(body))

        // Folding constant condition
        case If(cond, Seq(a1, a2), ct, cf) if a1 == a2 =>
          AppC(if (sameArgReduceC(cond)) ct else cf, Seq())

        case If(cond, args, ct, cf) if cEvaluator.isDefinedAt(cond, args.flatMap(s.lEnv.get)) =>
          val argsValues = args.flatMap(s.lEnv.get)
          val condV = cEvaluator(cond, argsValues)

          if (condV) AppC(ct, Seq())
          else AppC(cf, Seq())

        // Removing dead functions
        case LetF(fun, body) if fun.exists(f => s.dead(f.name)) =>
          shrinkT(LetF(fun.filter(f => !s.dead(f.name)), body))

        // Marking linear inlineable functions
        case LetF(fun, body) if fun.exists(f => s.appliedOnce(f.name)) =>
          val inlineable = fun.filter(f => s.appliedOnce(f.name))
          val newState = s.withEmptyInvEnvs.withFuns(inlineable)
          val notInlined = fun.diff(inlineable)
          shrinkT(LetF(notInlined, body))(newState)

        // Optimizing bodies of functions
        case LetF(fun, body) =>
          val optFuns = fun map { f =>
            val FunDef(name, retC, args, body) = f
            FunDef(name, retC, args, shrinkT(body))
          }

          // Optimize away when no functions are left
          if (optFuns.isEmpty) shrinkT(body)
          else LetF(optFuns, shrinkT(body))

        // Inlining continuation application
        case AppC(cnt, args) if s.cEnv.contains(cnt) =>
          val CntDef(_, fromArgs, body) = s.cEnv(cnt)
          shrinkT(body)(s.withSubst(fromArgs, args))

        // Inlining function application
        case AppF(fun, retC, args) if s.fEnv.contains(fun) =>
          val FunDef(_, c, fromArgs, body) = s.fEnv(fun)
          shrinkT(body)(s.withSubst(fromArgs, args))

        case _ =>
          // TODO
          tree
      }
      t.subst(s.subst)
    }
    shrinkT(tree)(State(census(tree)))
  }

  // (Non-shrinking) inlining

  private def inline(tree: Tree, maxSize: Int): Tree = {
    def copyT(tree: Tree, subst: Substitution[Name]): Tree = {
      (tree: @unchecked) match {
        case LetL(name, value, body) =>
          val name1 = name.copy
          LetL(name1, value, copyT(body, subst + (name -> name1)))
        case LetP(name, prim, args, body) =>
          val name1 = name.copy
          LetP(name1, prim, args map (subst(_)),
            copyT(body, subst + (name -> name1)))
        case LetC(cnts, body) =>
          val names = cnts map (_.name)
          val names1 = names map (_.copy)
          val subst1 = subst ++ (names zip names1)
          LetC(cnts map (copyC(_, subst1)), copyT(body, subst1))
        case LetF(funs, body) =>
          val names = funs map (_.name)
          val names1 = names map (_.copy)
          val subst1 = subst ++ (names zip names1)
          LetF(funs map (copyF(_, subst1)), copyT(body, subst1))
        case AppC(cnt, args) =>
          AppC(subst(cnt), args map (subst(_)))
        case AppF(fun, retC, args) =>
          AppF(subst(fun), subst(retC), args map (subst(_)))
        case If(cond, args, thenC, elseC) =>
          If(cond, args map (subst(_)), subst(thenC), subst(elseC))
            case Halt(arg) =>
          Halt(subst(arg))
      }
    }

    def copyC(cnt: CntDef, subst: Substitution[Name]): CntDef = {
      val args1 = cnt.args map (_.copy)
      val subst1 = subst ++ (cnt.args zip args1)
      CntDef(subst(cnt.name), args1, copyT(cnt.body, subst1))
    }

    def copyF(fun: FunDef, subst: Substitution[Name]): FunDef = {
      val retC1 = fun.retC.copy
      val args1 = fun.args map (_.copy)
      val subst1 = subst + (fun.retC -> retC1) ++ (fun.args zip args1)
      FunDef(subst(fun.name), retC1, args1, copyT(fun.body, subst1))
    }

    val fibonacci = Seq(1, 2, 3, 5, 8, 13)

    val trees = Stream.iterate((0, tree), fibonacci.length) { case (i, tree) =>
      val funLimit = fibonacci(i)
      val cntLimit = i

      def inlineT(tree: Tree)(implicit s: State): Tree = tree match {
        case _ =>
          // TODO
          tree
      }

      (i + 1, fixedPoint(inlineT(tree)(State(census(tree))))(shrink))
    }

    trees.takeWhile{ case (_, tree) => size(tree) <= maxSize }.last._2
  }

  // Census computation
  private def census(tree: Tree): Map[Name, Count] = {
    val census = MutableMap[Name, Count]()
    val rhs = MutableMap[Name, Tree]()

    def incAppUse(symbol: Name): Unit = {
      val currCount = census.getOrElse(symbol, Count())
      census(symbol) = currCount.copy(applied = currCount.applied + 1)
      rhs remove symbol foreach addToCensus
    }

    def incValUse(symbol: Name): Unit = {
      val currCount = census.getOrElse(symbol, Count())
      census(symbol) = currCount.copy(asValue = currCount.asValue + 1)
      rhs remove symbol foreach addToCensus
    }

    def addToCensus(tree: Tree): Unit = (tree: @unchecked) match {
      case LetL(_, _, body) =>
        addToCensus(body)
      case LetP(_, _, args, body) =>
        args foreach incValUse; addToCensus(body)
      case LetC(cnts, body) =>
        rhs ++= (cnts map { c => (c.name, c.body) }); addToCensus(body)
      case LetF(funs, body) =>
        rhs ++= (funs map { f => (f.name, f.body) }); addToCensus(body)
      case AppC(cnt, args) =>
        incAppUse(cnt); args foreach incValUse
      case AppF(fun, retC, args) =>
        incAppUse(fun); incValUse(retC); args foreach incValUse
      case If(_, args, thenC, elseC) =>
        args foreach incValUse; incValUse(thenC); incValUse(elseC)
      case Halt(arg) =>
        incValUse(arg)
    }

    addToCensus(tree)
    census.toMap
  }

  private def sameLen(formalArgs: Seq[Name], actualArgs: Seq[Name]): Boolean =
    formalArgs.length == actualArgs.length

  private def size(tree: Tree): Int = (tree: @unchecked) match {
    case LetL(_, _, body) => size(body) + 1
    case LetP(_, _, _, body) => size(body) + 1
    case LetC(cs, body) => (cs map { c => size(c.body) }).sum + size(body)
    case LetF(fs, body) => (fs map { f => size(f.body) }).sum + size(body)
    case AppC(_, _) | AppF(_, _, _) | If(_, _, _, _) | Halt(_) => 1
  }

  // Returns whether a ValuePrimitive has side-effects
  protected val impure: ValuePrimitive => Boolean
  // Returns whether different applications of a ValuePrimivite on the
  // same arguments may yield different results
  protected val unstable: ValuePrimitive => Boolean
  // Extracts the tag from a block allocation primitive
  protected val blockAllocTag: PartialFunction[ValuePrimitive, Literal]
  // Returns true for the block tag primitive
  protected val blockTag: ValuePrimitive
  // Returns true for the block length primitive
  protected val blockLength: ValuePrimitive
  // Returns true for the identity primitive
  protected val identity: ValuePrimitive

  // ValuePrimitives with their left-neutral elements
  protected val leftNeutral: Set[(Literal, ValuePrimitive)]
  // ValuePrimitives with their right-neutral elements
  protected val rightNeutral: Set[(ValuePrimitive, Literal)]
  // ValuePrimitives with their left-absorbing elements
  protected val leftAbsorbing: Set[(Literal, ValuePrimitive)]
  // ValuePrimitives with their right-absorbing elements
  protected val rightAbsorbing: Set[(ValuePrimitive, Literal)]
  // ValuePrimitives with the value equal arguments reduce to
  protected val sameArgReduce: PartialFunction[ValuePrimitive, Literal]
  // TestPrimitives with the (boolean) value equal arguments reduce to
  protected val sameArgReduceC: TestPrimitive => Boolean
  // An evaluator for ValuePrimitives
  protected val vEvaluator: PartialFunction[(ValuePrimitive, Seq[Literal]),
    Literal]
  // An evaluator for TestPrimitives
  protected val cEvaluator: PartialFunction[(TestPrimitive, Seq[Literal]),
    Boolean]
}

object CPSOptimizerHigh extends CPSOptimizer(SymbolicCPSTreeModule)
    with (SymbolicCPSTreeModule.Tree => SymbolicCPSTreeModule.Tree) {
  import treeModule._

  override def apply(t : Tree) = {
    val writer = new java.io.PrintWriter(System.err)
    val tree = super.apply(t)
    val fmt = new CPSTreeFormatter(SymbolicCPSTreeModule)
    fmt.toDocument(tree).format(80, writer)
    writer.println()
    fmt.toDocument(t).format(80, writer)
    writer.flush()
    tree
  }



  protected val impure: ValuePrimitive => Boolean =
    Set(L3ByteRead, L3ByteWrite, L3BlockSet) // TODO: Why not L3BlockAlloc ?

  protected val unstable: ValuePrimitive => Boolean =
    Set(L3BlockGet, L3ByteRead, L3BlockAlloc)

  protected val blockAllocTag: PartialFunction[ValuePrimitive, Literal] =
  { case L3BlockAlloc(tag) => IntLit(tag) }

  protected val blockTag: ValuePrimitive = L3BlockTag
  protected val blockLength: ValuePrimitive = L3BlockLength

  protected val identity: ValuePrimitive = L3Id

  protected val leftNeutral: Set[(Literal, ValuePrimitive)] = Set(
    IntLit(0) -> L3IntAdd,
    IntLit(1) -> L3IntMul,
    IntLit(0) -> L3IntArithShiftLeft,
    IntLit(0) -> L3IntArithShiftRight,
    IntLit(0) -> L3IntBitwiseOr,
    IntLit(~0) -> L3IntBitwiseAnd,
    IntLit(0) -> L3IntBitwiseXOr
  )

  protected val rightNeutral: Set[(ValuePrimitive, Literal)] = Set(
    L3IntAdd -> IntLit(0),
    L3IntMul -> IntLit(1),
    L3IntDiv -> IntLit(1),
    L3IntBitwiseOr -> IntLit(0),
    L3IntBitwiseXOr -> IntLit(0),
    L3IntArithShiftLeft -> IntLit(0),
    L3IntArithShiftRight -> IntLit(0),
    L3IntBitwiseAnd -> IntLit(~0)
  )

  protected val leftAbsorbing: Set[(Literal, ValuePrimitive)] = Set(
    IntLit(0) -> L3IntMul,
    IntLit(0) -> L3IntDiv,
    IntLit(0) -> L3IntBitwiseAnd,
    IntLit(~0) -> L3IntBitwiseOr
  )

  protected val rightAbsorbing: Set[(ValuePrimitive, Literal)] = Set(
    L3IntMul -> IntLit(0),
    L3IntBitwiseAnd -> IntLit(0),
    L3IntBitwiseOr -> IntLit(~0)
  )

  protected val sameArgReduce: PartialFunction[ValuePrimitive, Literal] = Map(
    L3IntSub -> IntLit(0),
    L3IntMod -> IntLit(0),
    L3IntDiv -> IntLit(1),
    L3IntBitwiseXOr -> IntLit(0)
  )

  protected val sameArgReduceC: PartialFunction[TestPrimitive, Boolean] = {
    case L3IntGe | L3IntLe | L3Eq => true
    case _ => false
  }
  //Map(L3IntGe -> true, L3IntLe -> true, L3Eq -> true).getOrElse(_, false)

  protected val vEvaluator: PartialFunction[(ValuePrimitive, Seq[Literal]),
    Literal] = {
    case (L3IntAdd, Seq(IntLit(x), IntLit(y))) => IntLit(x + y)
    case (L3IntMul, Seq(IntLit(x), IntLit(y))) => IntLit(x * y)
    case (L3IntDiv, Seq(IntLit(x), IntLit(y))) => IntLit(Math.floorDiv(x, y))
    case (L3IntMod, Seq(IntLit(x), IntLit(y))) => IntLit(Math.floorMod(x, y))
    case (L3IntBitwiseOr, Seq(IntLit(x), IntLit(y))) => IntLit(x | y)
    case (L3IntBitwiseAnd, Seq(IntLit(x), IntLit(y))) => IntLit(x & y)
    case (L3IntBitwiseXOr, Seq(IntLit(x), IntLit(y))) => IntLit(x ^ y)
    case (L3IntArithShiftLeft, Seq(IntLit(x), IntLit(y))) => IntLit(x << y)
    case (L3IntArithShiftRight, Seq(IntLit(x), IntLit(y))) => IntLit(x >> y)
  }


  protected val cEvaluator: PartialFunction[(TestPrimitive, Seq[Literal]),
    Boolean] = {
    //Type test primitives
    case (L3IntP, Seq(IntLit(_))) => true
    case (L3IntP, Seq(_)) => false
    case (L3CharP, Seq(CharLit(_))) => true
    case (L3CharP, Seq(_)) => false
    case (L3BoolP, Seq(BooleanLit(_))) => true
    case (L3BoolP, Seq(_)) => false
    case (L3UnitP, Seq(UnitLit)) => true
    case (L3UnitP, Seq(_)) => false
    case (L3BlockP, Seq(IntLit(_) | CharLit(_)
       | BooleanLit(_) | UnitLit)) => false

    // Arithmetic primitives
    case (L3IntGt, Seq(IntLit(x), IntLit(y))) => x > y
    case (L3IntGe, Seq(IntLit(x), IntLit(y))) => x >= y
    case (L3IntLt, Seq(IntLit(x), IntLit(y))) => x < y
    case (L3IntLe, Seq(IntLit(x), IntLit(y))) => x <= y
    case (L3Eq, Seq(x, y)) => x == y
    case (L3Ne, Seq(x, y)) => x != y
  }
}

object CPSOptimizerLow extends CPSOptimizer(SymbolicCPSTreeModuleLow)
    with (SymbolicCPSTreeModuleLow.Tree => SymbolicCPSTreeModuleLow.Tree) {
  import treeModule._

  protected val impure: ValuePrimitive => Boolean =
    Set(CPSBlockSet, CPSByteRead, CPSByteWrite)

  protected val unstable: ValuePrimitive => Boolean = {
    case CPSBlockAlloc(_) | CPSBlockGet | CPSByteRead => true
    case _ => false
  }

  protected val blockAllocTag: PartialFunction[ValuePrimitive, Literal] = {
    case CPSBlockAlloc(tag) => tag
  }
  protected val blockTag: ValuePrimitive = CPSBlockTag
  protected val blockLength: ValuePrimitive = CPSBlockLength

  protected val identity: ValuePrimitive = CPSId

  protected val leftNeutral: Set[(Literal, ValuePrimitive)] =
    Set((0, CPSAdd), (1, CPSMul), (~0, CPSAnd), (0, CPSOr), (0, CPSXOr))
  protected val rightNeutral: Set[(ValuePrimitive, Literal)] =
    Set((CPSAdd, 0), (CPSSub, 0), (CPSMul, 1), (CPSDiv, 1),
      (CPSArithShiftL, 0), (CPSArithShiftR, 0),
      (CPSAnd, ~0), (CPSOr, 0), (CPSXOr, 0))

  protected val leftAbsorbing: Set[(Literal, ValuePrimitive)] =
    Set((0, CPSMul), (0, CPSAnd), (~0, CPSOr))
  protected val rightAbsorbing: Set[(ValuePrimitive, Literal)] =
    Set((CPSMul, 0), (CPSAnd, 0), (CPSOr, ~0))

  protected val sameArgReduce: Map[ValuePrimitive, Literal] =
    Map(CPSSub -> 0, CPSDiv -> 1, CPSMod -> 0, CPSXOr -> 0)

  protected val sameArgReduceC: PartialFunction[TestPrimitive, Boolean] = {
    case CPSLe | CPSGe | CPSEq => true
    case CPSLt | CPSGt | CPSNe => false
  }

  protected val vEvaluator: PartialFunction[(ValuePrimitive, Seq[Literal]),
    Literal] = {
    case (CPSAdd, Seq(x, y)) => x + y
    case (CPSSub, Seq(x, y)) => x - y
    case (CPSMul, Seq(x, y)) => x * y
    case (CPSDiv, Seq(x, y)) if (y != 0) => Math.floorDiv(x, y)
    case (CPSMod, Seq(x, y)) if (y != 0) => Math.floorMod(x, y)

    case (CPSArithShiftL, Seq(x, y)) => x << y
    case (CPSArithShiftR, Seq(x, y)) => x >> y
    case (CPSAnd, Seq(x, y)) => x & y
    case (CPSOr, Seq(x, y)) => x | y
    case (CPSXOr, Seq(x, y)) => x ^ y
  }

  protected val cEvaluator: PartialFunction[(TestPrimitive, Seq[Literal]),
    Boolean] = {
    case (CPSLt, Seq(x, y)) => x < y
    case (CPSLe, Seq(x, y)) => x <= y
    case (CPSEq, Seq(x, y)) => x == y
    case (CPSNe, Seq(x, y)) => x != y
    case (CPSGe, Seq(x, y)) => x >= y
    case (CPSGt, Seq(x, y)) => x > y
  }
}

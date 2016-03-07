package l3

/**
  * Tree checker for CPS languages. Verifies that:
  *   1. names are globally unique (no name is bound more than once),
  *   2. names are used in their scope.
  *
  * @author Michel Schinz <Michel.Schinz@epfl.ch>
  */

abstract class CPSTreeChecker[T <: CPSTreeModule](treeModule: T)
    extends (T#Tree => Unit) {
  import treeModule._

  def apply(t: T#Tree): Unit = {
    val allNames = scala.collection.mutable.Set[Name]()

    def recordUniqueName(n: Name): Unit = {
      if (allNames contains n)
        error(s"Name ${n} is bound more than once (not globally unique).")
      else
        allNames += n
    }

    def checkName(n: Name, env: Set[Name]): Unit = {
      if (! (env contains n))
        error(s"Name ${n} is unbound.")
    }

    // @unchecked to suppress bogus warnings
    def checkT(t: T#Tree, cEnv: Set[Name], vEnv: Set[Name]): Unit = (t: @unchecked) match {
      case LetL(name, _, body) =>
        recordUniqueName(name)
        checkT(body, cEnv, vEnv + name)
      case LetP(name, _, args, body) =>
        recordUniqueName(name)
        args.foreach(checkName(_, vEnv))
        checkT(body, cEnv, vEnv + name)
      case LetC(cnts, body) =>
        val cEnv1 = cEnv ++ (cnts map (_.name))
        cnts.foreach(checkC(_, cEnv1, vEnv))
        checkT(body, cEnv1, vEnv)
      case LetF(funs, body) =>
        val vEnv1 = vEnv ++ (funs map (_.name))
        funs.foreach(checkF(_, vEnv1))
        checkT(body, cEnv, vEnv1)
      case AppC(cnt, args) =>
        checkName(cnt, cEnv)
        args.foreach(checkName(_, vEnv))
      case AppF(fun, retC, args) =>
        checkName(fun, vEnv)
        checkName(retC, cEnv)
        args.foreach(checkName(_, vEnv))
      case If(_, args, thenC, elseC) =>
        args.foreach(checkName(_, vEnv))
        checkName(thenC, cEnv)
        checkName(elseC, cEnv)
      case Halt(arg) =>
        checkName(arg, vEnv)
    }

    def checkC(cnt: CntDef, cEnv: Set[Name], vEnv: Set[Name]): Unit = {
      recordUniqueName(cnt.name)
      cnt.args.foreach(recordUniqueName)
      checkT(cnt.body, cEnv, vEnv ++ cnt.args)
    }

    def checkF(fun: FunDef, vEnv: Set[Name]): Unit = {
      recordUniqueName(fun.name)
      recordUniqueName(fun.retC)
      fun.args.foreach(recordUniqueName)
      checkT(fun.body, Set(fun.retC), vEnv ++ fun.args)
    }

    checkT(t, Set(), Set())
  }

  private def error(msg: String): Unit = {
    Console.println(s"Error: ${msg}")
  }
}

object SymbolicCPSTreeChecker
    extends CPSTreeChecker(SymbolicCPSTreeModule)

object SymbolicCPSTreeLowChecker
    extends CPSTreeChecker(SymbolicCPSTreeModuleLow)


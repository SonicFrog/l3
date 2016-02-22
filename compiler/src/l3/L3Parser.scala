package l3

import NominalCL3TreeModule._
import fastparse.core.Parsed

/**
  * Parsing (including lexical analysis) for the L₃ language.
  *
  * @author Michel Schinz <Michel.Schinz@epfl.ch>
  */

object L3Parser {
  def parse(programText: String,
            indexToPosition: Int=>Position): Parsed[Tree] = {
    val parser = new S(indexToPosition)
    parser.program.parse(programText)
  }

  // Lexical analysis (for which whitespace is significant)
  private class L(indexToPosition: Int=>Position) {
    import fastparse.all._
    private implicit val indexToPositionView = indexToPosition

    // Literals
    private val sign = CharIn("+-")
    private val prefix2 = IgnoreCase("#b")
    private val prefix16 = IgnoreCase("#x")
    private val digit2 = CharIn("01")
    private val digit10 = CharIn("0123456789")
    private val digit16 = CharIn("0123456789abcdefABCDEF")
    private def isStringChar(c: Char): Boolean = (c != '\n' && c != '"')
    private val unicodeChar =
      (CharPred(!Character.isHighSurrogate(_))
         | (CharPred(Character.isHighSurrogate)
              ~ CharPred(Character.isLowSurrogate)))

    private val integer2 = (Index ~ prefix2 ~/ (sign.? ~ digit2.rep(1)).!)
      .map { case (i, s) => Lit(IntLit(Integer.parseInt(s, 2)))(i) }
    private val integer10 = (Index ~ (sign.? ~ digit10 ~/ digit10.rep).!)
      .map { case (i, s) => Lit(IntLit(Integer.parseInt(s, 10)))(i) }
    private val integer16 = (Index ~ prefix16 ~/ (sign.? ~ digit16.rep(1)).!)
      .map { case (i, s) => Lit(IntLit(Integer.parseInt(s, 16)))(i) }
    private val integer = integer2 | integer10 | integer16
    private val string = (Index ~ "\"" ~/ CharPred(isStringChar).rep.! ~ "\"")
      .map { case (i, s) => sStringLit(s)(i) }
    private val char = (Index ~ "'" ~/ unicodeChar.! ~ "'")
      .map { case (i, c) => Lit(CharLit(c.codePointAt(0)))(i) }
    private val bool = (Index ~ "#" ~ CharIn("tf").!)
      .map { case (i, v) => Lit(BooleanLit(v == "t"))(i) }
    private val unit = (Index ~ "#u")
      .map { case i => Lit(UnitLit)(i) }

    val literal = P(integer | string | char | bool | unit)

    // Identifiers
    private val identStart = CharIn("|!%&*+-./:<=>?^_~"
                                      + "abcdefghijklmnopqrstuvwxyz"
                                      + "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
    private val identCont = identStart | sign | digit10
    private val identSuffix = "@" ~ digit10.rep(1)

    val identStr = (identStart ~/ identCont.rep ~/ identSuffix.?).!
    val identifier = (Index ~ identStr).map { case (i, n) => Ident(n)(i) }

    // Keywords
    private def kw(n: String): Parser[Unit] =
      n ~ !identCont

    val kDef = kw("def")
    val kDefrec = kw("defrec")
    val kFun = kw("fun")
    val kLet = kw("let")
    val kLet_* = kw("let*")
    val kLetrec = kw("letrec")
    val kRec = kw("rec")
    val kBegin = kw("begin")
    val kCond = kw("cond")
    val kIf = kw("if")
    val kAnd = kw("and")
    val kOr = kw("or")
    val kNot = kw("not")
    val kHalt = kw("halt")
    val kPrim = P("@")
  }

  // Syntactic analysis (for which whitespace and comments are ignored)
  private class S(indexToPosition: Int=>Position) {
    val White = fastparse.WhitespaceApi.Wrapper {
      import fastparse.all._
      (CharIn(" \t\n\r")
         | (";" ~ CharPred(c => c != '\n' && c != '\r').rep)).rep
    }
    import White._
    import fastparse.noApi._

    val lexer = new L(indexToPosition)
    import lexer._

    private implicit val indexToPositionView = indexToPosition

    val program: Parser[Tree] =
      P("" ~ topExpr ~ End) // The initial "" allows leading whitespace

    private val topExpr: Parser[Tree] = P(defP | defrecP | exprP)

    private val defP = P(iPar(kDef ~ identStr ~ expr) ~ topExpr)
      .map { case (i, (n, v), p) => Let(Seq((n, v)), p)(i) }
    private val defrecP = P(iPar(kDefrec ~ identStr ~ anonFun) ~ topExpr)
      .map { case (i, (n, (a, b)), p) => LetRec(Seq(FunDef(n, a, b)(i)), p)(i) }
    private val exprP = P(ix(expr ~ topExpr.?))
      .map { case (i, (e, p)) => sBegin(e +: p.toSeq)(i) }

    private val expr = P(fun | let | let_* | letrec | rec | begin
                           | cond | if_ | and | or | not
                           | halt | app | prim
                           | literal | identifier)
    private val exprs = expr.rep
    private val iExprs = ix(exprs)

    private val anonFun = par("fun" ~ par(identStr.rep) ~ ix(exprs))
      .map { case (a, (i, e)) => (a, sBegin(e)(i)) }
    private val funDef = iPar(identStr ~ anonFun)
      .map { case (i, (n, (a, e))) => FunDef(n, a, e)(i) }
    private val binding = par(identStr ~ expr)
      .map { case (i, e) => (i, e) }
    private val bindings =
      par(binding.rep)

    private val fun: Parser[Tree] = ix(anonFun)
      .map { case (i, (a, e)) => sFun(a, e)(i) }
    private val let: Parser[Tree] = iPar(kLet ~/ bindings ~ iExprs)
      .map { case (i1, (b, (i2, e))) => Let(b, sBegin(e)(i2))(i1) }
    private val let_* : Parser[Tree] = iPar(kLet_* ~/ bindings ~ iExprs)
      .map { case (i1, (b, (i2, e))) => sLet_*(b, sBegin(e)(i2))(i1) }
    private val letrec: Parser[Tree] = iPar(kLetrec ~/ par(funDef.rep) ~ iExprs)
      .map { case (i1, (f, (i2, e))) => LetRec(f, sBegin(e)(i2))(i1) }
    private val rec: Parser[Tree] = iPar(kRec ~/ identStr ~ bindings ~ iExprs)
      .map { case (i1, (n, b, (i2, e))) => sRec(n, b, sBegin(e)(i2))(i1) }
    private val begin: Parser[Tree] = iPar(kBegin ~/ exprs)
      .map { case (i, e) => sBegin(e)(i) }

    private val cond: Parser[Tree] = iPar(kCond ~/ par(expr ~ exprs).rep(1))
      .map { case (i, a) => sCond(a)(i) }
    private val if_ : Parser[Tree] = iPar(kIf ~ expr ~ expr ~ expr.?)
      .map { case (i, (c, t, f)) => If(c, t, f.getOrElse(Lit(UnitLit)(i)))(i) }
    private val and: Parser[Tree] = iPar(kAnd ~/ expr.rep(2))
      .map { case (i, es) => sAnd(es)(i) }
    private val or: Parser[Tree] = iPar(kOr ~/ expr.rep(2))
      .map { case (i, es) => sOr(es)(i) }
    private val not: Parser[Tree] = iPar(kNot ~/ expr)
      .map { case (i, e) => sNot(e)(i) }

    private val app: Parser[Tree] = iPar(expr ~ exprs)
      .map { case (i, (e, es)) => App(e, es)(i) }
    private val prim: Parser[Tree] = iPar(kPrim ~/ identStr ~ exprs)
      .map { case (i, (p, es)) => Prim(p, es)(i) }
    private val halt: Parser[Tree] = iPar(kHalt ~/ expr)
      .map { case (i, e) => Halt(e)(i) }

    private def par[T](p: Parser[T]): Parser[T] =
      "(" ~ p ~ ")"
    private def ix[T](p: Parser[T]): Parser[(Int, T)] =
      Index ~ p
    private def iPar[T](p: Parser[T]): Parser[(Int, T)] =
      ix(par(p))
  }

  // Syntactic sugar translation.
  private var freshCounter = 0
  private def freshName(prefix: String): String = {
    freshCounter += 1
    prefix + "$" + freshCounter
  }

  // TODO: We give you the hardest case, complete the rest!
  private def sStringLit(s: String)(implicit p: Position): Tree = {
    val b = freshName("string")
    val cs = codePoints(s)
    Let(Seq((b, Prim("block-alloc-"+ BlockTag.String.id,
                     Seq(Lit(IntLit(cs.length)))))),
        sBegin((cs.zipWithIndex map {case (c, i) =>
                  Prim("block-set!",
                       Seq(Ident(b), Lit(IntLit(i)), Lit(CharLit(c)))) })
                 :+ Ident(b)))
  }

  private def sFun(args: Seq[String], body: Tree)
                  (implicit p: Position): Tree = ???
  private def sLet_*(bdgs: Seq[(String,Tree)], body: Tree)
                    (implicit p: Position): Tree = ???
  private def sBegin(exprs: Seq[Tree])(implicit p: Position): Tree = ???
  private def sRec(name: String, bdgs: Seq[(String, Tree)], body: Tree)
                  (implicit p: Position) = ???
  private def sAnd(es: Seq[Tree])(implicit p: Position): Tree = ???
  private def sOr(es: Seq[Tree])(implicit p: Position): Tree = ???
  private def sNot(e: Tree)(implicit p: Position): Tree = ???
  private def sCond(clses: Seq[(Tree, Seq[Tree])])(implicit p: Position): Tree = ???
  private def codePoints(chars: Seq[Char]): Seq[Int] = ???
}

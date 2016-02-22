package l3

import prettyprint._

class CL3TreeFormatter[T <: CL3TreeModule](treeModule: T)
    extends Formatter[T#Tree] {
  import Formatter._
  import treeModule._

  def toDocument(tree: T#Tree): Document = {
    def funDefToDoc(funDef: FunDef): Document = {
      val argsDoc = pSeqToDoc(funDef.args, anyToDoc)
      paren(anyToDoc(funDef.name)
              :/: taggedParen2("fun", argsDoc, toDocument(funDef.body)))
    }

    (tree: @unchecked) match {
      case Let(bdgs, body) =>
        def bdgToDoc(b: (Name, Tree)): Document =
          paren(anyToDoc(b._1) :/: toDocument(b._2))
        taggedParen2("let", pSeqToDoc(bdgs, bdgToDoc), toDocument(body))
      case LetRec(funs, body) =>
        taggedParen2("letrec", pSeqToDoc(funs, funDefToDoc), toDocument(body))
      case If(c, t, e) =>
        taggedParen2("if", toDocument(c), toDocument(t) :/: toDocument(e))
      case App(fun, args) =>
        pSeqToDoc(fun +: args, toDocument)
      case Halt(arg) =>
        taggedParen("halt", toDocument(arg))
      case Prim(prim, args) =>
        taggedParen("@", anyToDoc(prim) :/: seqToDoc(args, toDocument))
      case Ident(name) =>
        anyToDoc(name)
      case Lit(l) =>
        anyToDoc(l)
    }
  }
}

object CL3TreeFormatter {
  implicit object NominalCL3TreeFormatter
      extends CL3TreeFormatter(NominalCL3TreeModule)
  implicit object SymbolicCL3TreeFormatter
      extends CL3TreeFormatter(SymbolicCL3TreeModule)
}

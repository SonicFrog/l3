package l3

import prettyprint.Document

class CPSTreeFormatter[T <: CPSTreeModule](treeModule: T)
    extends Formatter[T#Tree] {
  import Formatter._
  import treeModule._

  def toDocument(tree: T#Tree): Document = {
    def sugaredLetDocs(tree: T#Tree): (Seq[Document], Document) = tree match {
      case LetL(name, value, body) =>
        val (bdgsDocs, bodyDoc) = sugaredLetDocs(body)
        (paren(anyToDoc(name) :/: anyToDoc(value)) +: bdgsDocs, bodyDoc)
      case LetP(name, prim, args, body) =>
        val argsDoc = seqToDoc(args, anyToDoc)
        val bdgDoc = paren(anyToDoc(name)
                             :/: taggedParen(prim.toString, argsDoc))
        val (bdgsDocs, bodyDoc) = sugaredLetDocs(body)
        (bdgDoc +: bdgsDocs, bodyDoc)
      case LetC(cnts, body) =>
        val (bdgsDocs, bodyDoc) = sugaredLetDocs(body)
        ((cnts map cntDefToDoc) ++ bdgsDocs, bodyDoc)
      case LetF(funs, body) =>
        val (bdgsDocs, bodyDoc) = sugaredLetDocs(body)
        ((funs map funDefToDoc) ++ bdgsDocs, bodyDoc)
      case other =>
        (Seq(), toDocument(other))
    }

    def cntDefToDoc(c: CntDef): Document = {
      val argsDoc = pSeqToDoc(c.args, anyToDoc)
      paren(anyToDoc(c.name)
              :/: taggedParen2("cnt", argsDoc, toDocument(c.body)))
    }

    def funDefToDoc(f: FunDef): Document = {
      val argsDoc = pSeqToDoc(f.retC +: f.args, anyToDoc)
      paren(anyToDoc(f.name)
              :/: taggedParen2("fun", argsDoc, toDocument(f.body)))
    }

    (tree: @unchecked) match {
      case LetL(_, _, _) | LetP(_, _, _, _) | LetC(_, _) | LetF(_, _) =>
        val (bdgsDocs, bodyDoc) = sugaredLetDocs(tree)
        val tag = if (bdgsDocs.length > 1) "let*" else "let"
        taggedParen2(tag, paren(foldDoc(bdgsDocs)), bodyDoc)
      case AppF(fun, retC, args) =>
        taggedParen(fun.toString, seqToDoc(retC +: args, anyToDoc))
      case AppC(cont, args) =>
        taggedParen(cont.toString, seqToDoc(args, anyToDoc))
      case If(p, args, thenC, elseC) =>
        taggedParen2("if",
                     paren(p.toString :/: seqToDoc(args, anyToDoc)),
                     anyToDoc(thenC) :/: anyToDoc(elseC))
      case Halt(arg) =>
        taggedParen("halt", anyToDoc(arg))
    }
  }
}

object CPSTreeFormatter {
  implicit object SymbolicCPSTreeFormatter
      extends CPSTreeFormatter(SymbolicCPSTreeModule)
}

package l3

import prettyprint.Document
import Document._

/**
 * Utility methods for formatting.
 *
 * @author Michel Schinz <Michel.Schinz@epfl.ch>
 */

trait Formatter[T] {
  def toDocument(value: T): Document
}

object Formatter {
  def paren(d: Document): Document =
    group(anyToDoc("(") :: nest(1, d) :: anyToDoc(")"))

  def taggedParen(tag: String, d: Document): Document =
    group(anyToDoc("(") :: tag :: anyToDoc(" ")
            :: nest(1 + tag.length + 1, d) :: anyToDoc(")"))

  def taggedParen2(tag: String, d1: Document, d2: Document) =
    taggedParen(tag, d1 :: nest(-tag.length, break :: d2))

  def foldDoc(docs: Seq[Document]): Document = docs match {
    case Seq() => empty
    case Seq(d) => d
    case Seq(d, ds @ _*) => d :/: foldDoc(ds)
  }

  def seqToDoc[T](docs: Seq[T], toDoc: T=>Document): Document =
    foldDoc(docs map toDoc)

  def pSeqToDoc[T](docs: Seq[T], toDoc: T=>Document): Document =
    paren(seqToDoc(docs, toDoc))

  def anyToDoc(v: Any): Document = text(v.toString)
}

package l3

/*
 * A simple class for graphs. Inspired by "Lazy Depth-First Search and
 * Linear Graph Algorithms in Haskell" by David J. King and John
 * Launchbury.
 *
 * @author Michel Schinz <Michel.Schinz@epfl.ch>
 */

final class Graph[N] private (adj: Map[N, Seq[N]]) {
  def stronglyConnectedComponents: Seq[Seq[N]] =
    depthFirstForest(reverse.postOrder.reverse) map postOrderT

  override def toString: String =
    adj.toString

  private def nodes: Seq[N] =
    adj.keys.toSeq

  private def reverse: Graph[N] = {
    val invEdges = for ((s, ds) <- adj.toSeq; d <- ds) yield (d, s)
    val invAdj = invEdges groupBy (_._1) mapValues { es => es map (_._2) }
    Graph((nodes map { case n => (n, invAdj.getOrElse(n, Seq())) }).toMap)
  }

  private type Forest = Seq[Tree]
  private case class Tree(value: N, children: Forest)

  private def depthFirstForest(ns: Seq[N]): Forest = {
    val visited = scala.collection.mutable.Set[N]()

    def depthFirstTree(n: N): Tree = {
      require(!visited(n))

      visited add n
      var c: Seq[Tree] = Seq.empty
      for (s <- adj(n))
        if (!visited(s))
          c = depthFirstTree(s) +: c
      Tree(n, c.reverse)
    }

    var f: Forest = Seq.empty
    for (n <- ns)
      if (!visited(n))
        f = depthFirstTree(n) +: f
    f.reverse
  }

  private def postOrder: Seq[N] =
    postOrderF(depthFirstForest(nodes))

  private def postOrderT(t: Tree): Seq[N] =
    t.value +: postOrderF(t.children)
  private def postOrderF(f: Forest): Seq[N] =
    f flatMap postOrderT
}

object Graph {
  def apply[N](adj: Map[N, Seq[N]]): Graph[N] = {
    val completeAdj = (adj /: adj.values.flatten) { case (adj, n) =>
      if (adj contains n) adj else adj + (n -> Seq.empty)
    }
    new Graph(completeAdj)
  }
}

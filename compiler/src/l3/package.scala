package object l3 {
  private def fixedPoint[T](start: T, maxIters: Option[Int])(f: T=>T): T = {
    val approx = Stream.iterate(start, maxIters getOrElse Integer.MAX_VALUE)(f)
    val (improv, stable) = ((approx zip approx.tail) span (p => p._1 != p._2))
    if (improv.isEmpty) stable.head._1 else improv.last._2
  }

  private[l3] def fixedPoint[T](start: T)(f: T=>T): T =
    fixedPoint(start, None)(f)

  private[l3] def fixedPoint[T](start: T, maxIters: Int)(f: T=>T): T =
    fixedPoint(start, Some(maxIters))(f)
}

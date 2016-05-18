package l3

sealed trait SingleElementSet[T] {
  def join(that: SingleElementSet[T]): SingleElementSet[T] =
    (this, that) match {
      case (Top(), _) | (_, Top()) => Top()
      case (Singleton(e1), Singleton(e2)) if e1 == e2 => Singleton(e1)
      case (Singleton(_), Singleton(_)) => Top()
      case (Bot(), s) => s
      case (s, Bot()) => s
    }
}

case class Top[T]() extends SingleElementSet[T]
case class Singleton[T](e: T) extends SingleElementSet[T]
case class Bot[T]() extends SingleElementSet[T]

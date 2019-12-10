package medit.editor


trait Mover {

  protected def root: Node


  def parent(focus: Seq[Int]): Option[Seq[Int]] = {
    if (focus.nonEmpty) {
      Some(focus.dropRight(1))
    } else {
      None
    }
  }

  def firstChild(a: Seq[Int]): Option[Seq[Int]] = {
    val size = root(a).childs.size
    if (size > 0) {
      Some(a :+ 0)
    } else {
      None
    }
  }

  def lastChild(a: Seq[Int]): Option[Seq[Int]] = {
    val size = root(a).childs.size
    if (size == 0) {
      None
    } else {
      Some(a :+ (size - 1))
    }
  }

  def previous(a: Seq[Int]): Option[Seq[Int]] = {
    if (a.nonEmpty) {
      val last = a.last
      if (last > 0) {
        Some(a.dropRight(1) :+ (last - 1))
      } else {
        None
      }
    } else {
      None
    }
  }

  def next(a: Seq[Int]): Option[Seq[Int]] = {
    if (a.isEmpty) {
      None
    } else {
      val last = a.last
      if (last < root(a.dropRight(1)).childs.size - 1) {
        Some(a.dropRight(1) :+ (last + 1))
      } else {
        None
      }
    }
  }

  def visualBottom(k: Seq[Int]): Seq[Int] = {
    lastChild(k) match {
      case Some(a) => visualBottom(a)
      case _ => k
    }
  }

  def visualUp(a: Seq[Int]): Option[Seq[Int]] = {
    previous(a) match {
      case None => parent(a)
      case Some(k) => Some(visualBottom(k))
    }
  }


  def globalNext(a: Seq[Int]): Option[Seq[Int]] = next(a).orElse(parent(a).flatMap(globalNext))

  def visualDown(a: Seq[Int]): Option[Seq[Int]] = {
    firstChild(a).orElse(globalNext(a))
  }
}
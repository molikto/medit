package medit.draw

trait Drawable {
  def draw(width: Int): Seq[DrawCall]
}

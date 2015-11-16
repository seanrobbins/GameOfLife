case class Coordinate(x: Int, y: Int) {
  def neighbourhood: Vector[Coordinate] = for {
    x <- Vector(x - 1, x, x + 1)
    y <- Vector(y - 1, y, y + 1)
  } yield Coordinate(x, y)

  def neighbours: Vector[Coordinate] = neighbourhood filterNot this.==
}
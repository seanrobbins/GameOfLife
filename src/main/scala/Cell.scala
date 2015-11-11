sealed trait Cell {
  val aliveNeighbours: Int
}
case class Alive(aliveNeighbours: Int) extends Cell
case class Dead(aliveNeighbours: Int) extends Cell

object Cell {
  def rules(c: Cell): Cell = c match {
    case Alive(a) if a < 2 || a >= 4 => new Dead(a)
    case Dead(a) if a == 3 => new Alive(a)
    case _ => c
  }
}
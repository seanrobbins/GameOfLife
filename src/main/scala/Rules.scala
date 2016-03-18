object Rules {
  def apply(c: Cell, liveNeighbours: Int): Cell = c match {
    case Alive if liveNeighbours < 2 || liveNeighbours >= 4 => Dead
    case Dead if liveNeighbours == 3 => Alive
    case _ => c
  }
}
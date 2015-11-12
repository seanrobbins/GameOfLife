class Grid(val vector: Vector[Vector[Cell]])

object Grid {
  def initialize(x: Int, y: Int): Grid = {
    new Grid(Vector.fill[Cell](x, y) { new Dead(0) })
  }

  def getCell(grid: Grid)(x: Int, y: Int): Cell = {
    grid.vector(x)(y)
  }

  def setAlive(grid: Grid)(x: Int, y: Int): Grid = {
    val updatedY: Vector[Cell] = grid.vector(x).updated(y, new Alive(0))
    new Grid(grid.vector.updated(x, updatedY))
  }

  def setDead(grid: Grid)(x: Int, y: Int): Grid = {
    val updatedY: Vector[Cell] = grid.vector(x).updated(y, new Dead(0))
    new Grid(grid.vector.updated(x, updatedY))
  }
}